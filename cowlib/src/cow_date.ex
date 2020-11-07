defmodule :cow_date do
  use Bitwise

  def parse_date(dateBin) do
    date = {{_, _, d}, {h, m, s}} = http_date(dateBin)
    true = d >= 0 and d <= 31
    true = h >= 0 and h <= 23
    true = m >= 0 and m <= 59
    true = s >= 0 and s <= 60
    date
  end

  defp http_date(<<"Mon, ", d1, d2, " ", r::bits>>) do
    fixdate(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Tue, ", d1, d2, " ", r::bits>>) do
    fixdate(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Wed, ", d1, d2, " ", r::bits>>) do
    fixdate(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Thu, ", d1, d2, " ", r::bits>>) do
    fixdate(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Fri, ", d1, d2, " ", r::bits>>) do
    fixdate(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Sat, ", d1, d2, " ", r::bits>>) do
    fixdate(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Sun, ", d1, d2, " ", r::bits>>) do
    fixdate(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Monday, ", d1, d2, "-", r::bits>>) do
    rfc850_date(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Tuesday, ", d1, d2, "-", r::bits>>) do
    rfc850_date(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Wednesday, ", d1, d2, "-", r::bits>>) do
    rfc850_date(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Thursday, ", d1, d2, "-", r::bits>>) do
    rfc850_date(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Friday, ", d1, d2, "-", r::bits>>) do
    rfc850_date(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Saturday, ", d1, d2, "-", r::bits>>) do
    rfc850_date(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Sunday, ", d1, d2, "-", r::bits>>) do
    rfc850_date(r, (d1 - ?0) * 10 + (d2 - ?0))
  end

  defp http_date(<<"Mon ", r::bits>>) do
    asctime_date(r)
  end

  defp http_date(<<"Tue ", r::bits>>) do
    asctime_date(r)
  end

  defp http_date(<<"Wed ", r::bits>>) do
    asctime_date(r)
  end

  defp http_date(<<"Thu ", r::bits>>) do
    asctime_date(r)
  end

  defp http_date(<<"Fri ", r::bits>>) do
    asctime_date(r)
  end

  defp http_date(<<"Sat ", r::bits>>) do
    asctime_date(r)
  end

  defp http_date(<<"Sun ", r::bits>>) do
    asctime_date(r)
  end

  defp fixdate(
         <<"Jan ", y1, y2, y3, y4, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 1, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp fixdate(
         <<"Feb ", y1, y2, y3, y4, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 2, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp fixdate(
         <<"Mar ", y1, y2, y3, y4, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 3, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp fixdate(
         <<"Apr ", y1, y2, y3, y4, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 4, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp fixdate(
         <<"May ", y1, y2, y3, y4, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 5, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp fixdate(
         <<"Jun ", y1, y2, y3, y4, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 6, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp fixdate(
         <<"Jul ", y1, y2, y3, y4, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 7, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp fixdate(
         <<"Aug ", y1, y2, y3, y4, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 8, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp fixdate(
         <<"Sep ", y1, y2, y3, y4, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 9, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp fixdate(
         <<"Oct ", y1, y2, y3, y4, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 10, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp fixdate(
         <<"Nov ", y1, y2, y3, y4, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 11, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp fixdate(
         <<"Dec ", y1, y2, y3, y4, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 12, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_date(
         <<"Jan-", y1, y2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{rfc850_year((y1 - ?0) * 10 + (y2 - ?0)), 1, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_date(
         <<"Feb-", y1, y2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{rfc850_year((y1 - ?0) * 10 + (y2 - ?0)), 2, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_date(
         <<"Mar-", y1, y2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{rfc850_year((y1 - ?0) * 10 + (y2 - ?0)), 3, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_date(
         <<"Apr-", y1, y2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{rfc850_year((y1 - ?0) * 10 + (y2 - ?0)), 4, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_date(
         <<"May-", y1, y2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{rfc850_year((y1 - ?0) * 10 + (y2 - ?0)), 5, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_date(
         <<"Jun-", y1, y2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{rfc850_year((y1 - ?0) * 10 + (y2 - ?0)), 6, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_date(
         <<"Jul-", y1, y2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{rfc850_year((y1 - ?0) * 10 + (y2 - ?0)), 7, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_date(
         <<"Aug-", y1, y2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{rfc850_year((y1 - ?0) * 10 + (y2 - ?0)), 8, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_date(
         <<"Sep-", y1, y2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{rfc850_year((y1 - ?0) * 10 + (y2 - ?0)), 9, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_date(
         <<"Oct-", y1, y2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{rfc850_year((y1 - ?0) * 10 + (y2 - ?0)), 10, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_date(
         <<"Nov-", y1, y2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{rfc850_year((y1 - ?0) * 10 + (y2 - ?0)), 11, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_date(
         <<"Dec-", y1, y2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " GMT">>,
         day
       ) do
    {{rfc850_year((y1 - ?0) * 10 + (y2 - ?0)), 12, day},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp rfc850_year(y) when y > 50 do
    y + 1900
  end

  defp rfc850_year(y) do
    y + 2000
  end

  defp asctime_date(
         <<"Jan ", d1, d2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " ", y1, y2, y3, y4>>
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 1, asctime_day(d1, d2)},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp asctime_date(
         <<"Feb ", d1, d2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " ", y1, y2, y3, y4>>
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 2, asctime_day(d1, d2)},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp asctime_date(
         <<"Mar ", d1, d2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " ", y1, y2, y3, y4>>
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 3, asctime_day(d1, d2)},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp asctime_date(
         <<"Apr ", d1, d2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " ", y1, y2, y3, y4>>
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 4, asctime_day(d1, d2)},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp asctime_date(
         <<"May ", d1, d2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " ", y1, y2, y3, y4>>
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 5, asctime_day(d1, d2)},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp asctime_date(
         <<"Jun ", d1, d2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " ", y1, y2, y3, y4>>
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 6, asctime_day(d1, d2)},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp asctime_date(
         <<"Jul ", d1, d2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " ", y1, y2, y3, y4>>
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 7, asctime_day(d1, d2)},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp asctime_date(
         <<"Aug ", d1, d2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " ", y1, y2, y3, y4>>
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 8, asctime_day(d1, d2)},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp asctime_date(
         <<"Sep ", d1, d2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " ", y1, y2, y3, y4>>
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 9, asctime_day(d1, d2)},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp asctime_date(
         <<"Oct ", d1, d2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " ", y1, y2, y3, y4>>
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 10, asctime_day(d1, d2)},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp asctime_date(
         <<"Nov ", d1, d2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " ", y1, y2, y3, y4>>
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 11, asctime_day(d1, d2)},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp asctime_date(
         <<"Dec ", d1, d2, " ", h1, h2, ":", m1, m2, ":", s1, s2, " ", y1, y2, y3, y4>>
       ) do
    {{(y1 - ?0) * 1000 + (y2 - ?0) * 100 + (y3 - ?0) * 10 + (y4 - ?0), 12, asctime_day(d1, d2)},
     {(h1 - ?0) * 10 + (h2 - ?0), (m1 - ?0) * 10 + (m2 - ?0), (s1 - ?0) * 10 + (s2 - ?0)}}
  end

  defp asctime_day(?\s, d2) do
    d2 - ?0
  end

  defp asctime_day(d1, d2) do
    (d1 - ?0) * 10 + (d2 - ?0)
  end

  def rfc1123(dateTime) do
    rfc7231(dateTime)
  end

  def rfc2109({date = {y, mo, d}, {h, mi, s}}) do
    wday = :calendar.day_of_the_week(date)

    <<weekday(wday)::binary, ", ", pad_int(d)::binary, "-", month(mo)::binary, "-",
      year(y)::binary, " ", pad_int(h)::binary, ":", pad_int(mi)::binary, ":", pad_int(s)::binary,
      " GMT">>
  end

  def rfc7231({date = {y, mo, d}, {h, mi, s}}) do
    wday = :calendar.day_of_the_week(date)

    <<weekday(wday)::binary, ", ", pad_int(d)::binary, " ", month(mo)::binary, " ",
      year(y)::binary, " ", pad_int(h)::binary, ":", pad_int(mi)::binary, ":", pad_int(s)::binary,
      " GMT">>
  end

  defp pad_int(0) do
    "00"
  end

  defp pad_int(1) do
    "01"
  end

  defp pad_int(2) do
    "02"
  end

  defp pad_int(3) do
    "03"
  end

  defp pad_int(4) do
    "04"
  end

  defp pad_int(5) do
    "05"
  end

  defp pad_int(6) do
    "06"
  end

  defp pad_int(7) do
    "07"
  end

  defp pad_int(8) do
    "08"
  end

  defp pad_int(9) do
    "09"
  end

  defp pad_int(10) do
    "10"
  end

  defp pad_int(11) do
    "11"
  end

  defp pad_int(12) do
    "12"
  end

  defp pad_int(13) do
    "13"
  end

  defp pad_int(14) do
    "14"
  end

  defp pad_int(15) do
    "15"
  end

  defp pad_int(16) do
    "16"
  end

  defp pad_int(17) do
    "17"
  end

  defp pad_int(18) do
    "18"
  end

  defp pad_int(19) do
    "19"
  end

  defp pad_int(20) do
    "20"
  end

  defp pad_int(21) do
    "21"
  end

  defp pad_int(22) do
    "22"
  end

  defp pad_int(23) do
    "23"
  end

  defp pad_int(24) do
    "24"
  end

  defp pad_int(25) do
    "25"
  end

  defp pad_int(26) do
    "26"
  end

  defp pad_int(27) do
    "27"
  end

  defp pad_int(28) do
    "28"
  end

  defp pad_int(29) do
    "29"
  end

  defp pad_int(30) do
    "30"
  end

  defp pad_int(31) do
    "31"
  end

  defp pad_int(32) do
    "32"
  end

  defp pad_int(33) do
    "33"
  end

  defp pad_int(34) do
    "34"
  end

  defp pad_int(35) do
    "35"
  end

  defp pad_int(36) do
    "36"
  end

  defp pad_int(37) do
    "37"
  end

  defp pad_int(38) do
    "38"
  end

  defp pad_int(39) do
    "39"
  end

  defp pad_int(40) do
    "40"
  end

  defp pad_int(41) do
    "41"
  end

  defp pad_int(42) do
    "42"
  end

  defp pad_int(43) do
    "43"
  end

  defp pad_int(44) do
    "44"
  end

  defp pad_int(45) do
    "45"
  end

  defp pad_int(46) do
    "46"
  end

  defp pad_int(47) do
    "47"
  end

  defp pad_int(48) do
    "48"
  end

  defp pad_int(49) do
    "49"
  end

  defp pad_int(50) do
    "50"
  end

  defp pad_int(51) do
    "51"
  end

  defp pad_int(52) do
    "52"
  end

  defp pad_int(53) do
    "53"
  end

  defp pad_int(54) do
    "54"
  end

  defp pad_int(55) do
    "55"
  end

  defp pad_int(56) do
    "56"
  end

  defp pad_int(57) do
    "57"
  end

  defp pad_int(58) do
    "58"
  end

  defp pad_int(59) do
    "59"
  end

  defp pad_int(60) do
    "60"
  end

  defp pad_int(int) do
    :erlang.integer_to_binary(int)
  end

  defp weekday(1) do
    "Mon"
  end

  defp weekday(2) do
    "Tue"
  end

  defp weekday(3) do
    "Wed"
  end

  defp weekday(4) do
    "Thu"
  end

  defp weekday(5) do
    "Fri"
  end

  defp weekday(6) do
    "Sat"
  end

  defp weekday(7) do
    "Sun"
  end

  defp month(1) do
    "Jan"
  end

  defp month(2) do
    "Feb"
  end

  defp month(3) do
    "Mar"
  end

  defp month(4) do
    "Apr"
  end

  defp month(5) do
    "May"
  end

  defp month(6) do
    "Jun"
  end

  defp month(7) do
    "Jul"
  end

  defp month(8) do
    "Aug"
  end

  defp month(9) do
    "Sep"
  end

  defp month(10) do
    "Oct"
  end

  defp month(11) do
    "Nov"
  end

  defp month(12) do
    "Dec"
  end

  defp year(1970) do
    "1970"
  end

  defp year(1971) do
    "1971"
  end

  defp year(1972) do
    "1972"
  end

  defp year(1973) do
    "1973"
  end

  defp year(1974) do
    "1974"
  end

  defp year(1975) do
    "1975"
  end

  defp year(1976) do
    "1976"
  end

  defp year(1977) do
    "1977"
  end

  defp year(1978) do
    "1978"
  end

  defp year(1979) do
    "1979"
  end

  defp year(1980) do
    "1980"
  end

  defp year(1981) do
    "1981"
  end

  defp year(1982) do
    "1982"
  end

  defp year(1983) do
    "1983"
  end

  defp year(1984) do
    "1984"
  end

  defp year(1985) do
    "1985"
  end

  defp year(1986) do
    "1986"
  end

  defp year(1987) do
    "1987"
  end

  defp year(1988) do
    "1988"
  end

  defp year(1989) do
    "1989"
  end

  defp year(1990) do
    "1990"
  end

  defp year(1991) do
    "1991"
  end

  defp year(1992) do
    "1992"
  end

  defp year(1993) do
    "1993"
  end

  defp year(1994) do
    "1994"
  end

  defp year(1995) do
    "1995"
  end

  defp year(1996) do
    "1996"
  end

  defp year(1997) do
    "1997"
  end

  defp year(1998) do
    "1998"
  end

  defp year(1999) do
    "1999"
  end

  defp year(2000) do
    "2000"
  end

  defp year(2001) do
    "2001"
  end

  defp year(2002) do
    "2002"
  end

  defp year(2003) do
    "2003"
  end

  defp year(2004) do
    "2004"
  end

  defp year(2005) do
    "2005"
  end

  defp year(2006) do
    "2006"
  end

  defp year(2007) do
    "2007"
  end

  defp year(2008) do
    "2008"
  end

  defp year(2009) do
    "2009"
  end

  defp year(2010) do
    "2010"
  end

  defp year(2011) do
    "2011"
  end

  defp year(2012) do
    "2012"
  end

  defp year(2013) do
    "2013"
  end

  defp year(2014) do
    "2014"
  end

  defp year(2015) do
    "2015"
  end

  defp year(2016) do
    "2016"
  end

  defp year(2017) do
    "2017"
  end

  defp year(2018) do
    "2018"
  end

  defp year(2019) do
    "2019"
  end

  defp year(2020) do
    "2020"
  end

  defp year(2021) do
    "2021"
  end

  defp year(2022) do
    "2022"
  end

  defp year(2023) do
    "2023"
  end

  defp year(2024) do
    "2024"
  end

  defp year(2025) do
    "2025"
  end

  defp year(2026) do
    "2026"
  end

  defp year(2027) do
    "2027"
  end

  defp year(2028) do
    "2028"
  end

  defp year(2029) do
    "2029"
  end

  defp year(year) do
    :erlang.integer_to_binary(year)
  end
end
