defmodule :cow_http do
  use Bitwise

  def parse_request_line(data) do
    {pos, _} = :binary.match(data, "\r")
    <<requestLine::size(pos)-binary, "\r\n", rest::bits>> = data
    [method, target, version0] = :binary.split(requestLine, <<?\s>>, [:trim_all, :global])

    version =
      case version0 do
        "HTTP/1.1" ->
          :"HTTP/1.1"

        "HTTP/1.0" ->
          :"HTTP/1.0"
      end

    {method, target, version, rest}
  end

  def parse_status_line(<<"HTTP/1.1 200 OK\r\n", rest::bits>>) do
    {:"HTTP/1.1", 200, "OK", rest}
  end

  def parse_status_line(<<"HTTP/1.1 404 Not Found\r\n", rest::bits>>) do
    {:"HTTP/1.1", 404, "Not Found", rest}
  end

  def parse_status_line(<<"HTTP/1.1 500 Internal Server Error\r\n", rest::bits>>) do
    {:"HTTP/1.1", 500, "Internal Server Error", rest}
  end

  def parse_status_line(<<"HTTP/1.1 ", status::bits>>) do
    parse_status_line(status, :"HTTP/1.1")
  end

  def parse_status_line(<<"HTTP/1.0 ", status::bits>>) do
    parse_status_line(status, :"HTTP/1.0")
  end

  defp parse_status_line(<<h, t, u, " ", rest::bits>>, version) do
    status = status_to_integer(h, t, u)
    {pos, _} = :binary.match(rest, "\r")
    <<statusStr::size(pos)-binary, "\r\n", rest2::bits>> = rest
    {version, status, statusStr, rest2}
  end

  def status_to_integer(status) when is_integer(status) do
    status
  end

  def status_to_integer(status) do
    case status do
      <<h, t, u>> ->
        status_to_integer(h, t, u)

      <<h, t, u, " ", _::bits>> ->
        status_to_integer(h, t, u)
    end
  end

  defp status_to_integer(h, t, u)
       when ?0 <= h and h <= ?9 and
              ?0 <= t and t <= ?9 and ?0 <= u and u <= ?9 do
    (h - ?0) * 100 + (t - ?0) * 10 + (u - ?0)
  end

  def parse_headers(data) do
    parse_header(data, [])
  end

  defp parse_header(<<?\r, ?\n, rest::bits>>, acc) do
    {:lists.reverse(acc), rest}
  end

  defp parse_header(data, acc) do
    parse_hd_name(data, acc, <<>>)
  end

  defp parse_hd_name(<<c, rest::bits>>, acc, soFar) do
    case c do
      ?: ->
        parse_hd_before_value(rest, acc, soFar)

      ?\s ->
        parse_hd_name_ws(rest, acc, soFar)

      ?\t ->
        parse_hd_name_ws(rest, acc, soFar)

      _ ->
        case c do
          ?A ->
            parse_hd_name(rest, acc, <<soFar::binary, ?a>>)

          ?B ->
            parse_hd_name(rest, acc, <<soFar::binary, ?b>>)

          ?C ->
            parse_hd_name(rest, acc, <<soFar::binary, ?c>>)

          ?D ->
            parse_hd_name(rest, acc, <<soFar::binary, ?d>>)

          ?E ->
            parse_hd_name(rest, acc, <<soFar::binary, ?e>>)

          ?F ->
            parse_hd_name(rest, acc, <<soFar::binary, ?f>>)

          ?G ->
            parse_hd_name(rest, acc, <<soFar::binary, ?g>>)

          ?H ->
            parse_hd_name(rest, acc, <<soFar::binary, ?h>>)

          ?I ->
            parse_hd_name(rest, acc, <<soFar::binary, ?i>>)

          ?J ->
            parse_hd_name(rest, acc, <<soFar::binary, ?j>>)

          ?K ->
            parse_hd_name(rest, acc, <<soFar::binary, ?k>>)

          ?L ->
            parse_hd_name(rest, acc, <<soFar::binary, ?l>>)

          ?M ->
            parse_hd_name(rest, acc, <<soFar::binary, ?m>>)

          ?N ->
            parse_hd_name(rest, acc, <<soFar::binary, ?n>>)

          ?O ->
            parse_hd_name(rest, acc, <<soFar::binary, ?o>>)

          ?P ->
            parse_hd_name(rest, acc, <<soFar::binary, ?p>>)

          ?Q ->
            parse_hd_name(rest, acc, <<soFar::binary, ?q>>)

          ?R ->
            parse_hd_name(rest, acc, <<soFar::binary, ?r>>)

          ?S ->
            parse_hd_name(rest, acc, <<soFar::binary, ?s>>)

          ?T ->
            parse_hd_name(rest, acc, <<soFar::binary, ?t>>)

          ?U ->
            parse_hd_name(rest, acc, <<soFar::binary, ?u>>)

          ?V ->
            parse_hd_name(rest, acc, <<soFar::binary, ?v>>)

          ?W ->
            parse_hd_name(rest, acc, <<soFar::binary, ?w>>)

          ?X ->
            parse_hd_name(rest, acc, <<soFar::binary, ?x>>)

          ?Y ->
            parse_hd_name(rest, acc, <<soFar::binary, ?y>>)

          ?Z ->
            parse_hd_name(rest, acc, <<soFar::binary, ?z>>)

          ^c ->
            parse_hd_name(rest, acc, <<soFar::binary, c>>)
        end
    end
  end

  defp parse_hd_name_ws(<<c, rest::bits>>, acc, name) do
    case c do
      ?: ->
        parse_hd_before_value(rest, acc, name)

      ?\s ->
        parse_hd_name_ws(rest, acc, name)

      ?\t ->
        parse_hd_name_ws(rest, acc, name)
    end
  end

  defp parse_hd_before_value(<<?\s, rest::bits>>, acc, name) do
    parse_hd_before_value(rest, acc, name)
  end

  defp parse_hd_before_value(<<?\t, rest::bits>>, acc, name) do
    parse_hd_before_value(rest, acc, name)
  end

  defp parse_hd_before_value(data, acc, name) do
    parse_hd_value(data, acc, name, <<>>)
  end

  defp parse_hd_value(<<?\r, rest::bits>>, acc, name, soFar) do
    case rest do
      <<?\n, c, rest2::bits>> when c === ?\s or c === ?\t ->
        parse_hd_value(rest2, acc, name, <<soFar::binary, c>>)

      <<?\n, rest2::bits>> ->
        value = clean_value_ws_end(soFar, byte_size(soFar) - 1)
        parse_header(rest2, [{name, value} | acc])
    end
  end

  defp parse_hd_value(<<c, rest::bits>>, acc, name, soFar) do
    parse_hd_value(rest, acc, name, <<soFar::binary, c>>)
  end

  defp clean_value_ws_end(_, -1) do
    <<>>
  end

  defp clean_value_ws_end(value, n) do
    case :binary.at(value, n) do
      ?\s ->
        clean_value_ws_end(value, n - 1)

      ?\t ->
        clean_value_ws_end(value, n - 1)

      _ ->
        s = n + 1
        <<value2::size(s)-binary, _::bits>> = value
        value2
    end
  end

  def parse_fullpath(fullpath) do
    parse_fullpath(fullpath, <<>>)
  end

  defp parse_fullpath(<<>>, path) do
    {path, <<>>}
  end

  defp parse_fullpath(<<?#, _::bits>>, path) do
    {path, <<>>}
  end

  defp parse_fullpath(<<??, qs::bits>>, path) do
    parse_fullpath_query(qs, path, <<>>)
  end

  defp parse_fullpath(<<c, rest::bits>>, soFar) do
    parse_fullpath(rest, <<soFar::binary, c>>)
  end

  defp parse_fullpath_query(<<>>, path, query) do
    {path, query}
  end

  defp parse_fullpath_query(<<?#, _::bits>>, path, query) do
    {path, query}
  end

  defp parse_fullpath_query(<<c, rest::bits>>, path, soFar) do
    parse_fullpath_query(rest, path, <<soFar::binary, c>>)
  end

  def parse_version("HTTP/1.1") do
    :"HTTP/1.1"
  end

  def parse_version("HTTP/1.0") do
    :"HTTP/1.0"
  end

  def request(method, path, version, headers) do
    [
      method,
      " ",
      path,
      " ",
      version(version),
      "\r\n",
      for {n, v} <- headers do
        [n, ": ", v, "\r\n"]
      end,
      "\r\n"
    ]
  end

  def response(status, version, headers) do
    [version(version), " ", status(status), "\r\n", headers(headers), "\r\n"]
  end

  def headers(headers) do
    for {n, v} <- headers do
      [n, ": ", v, "\r\n"]
    end
  end

  def version(:"HTTP/1.1") do
    "HTTP/1.1"
  end

  def version(:"HTTP/1.0") do
    "HTTP/1.0"
  end

  defp status(100) do
    "100 Continue"
  end

  defp status(101) do
    "101 Switching Protocols"
  end

  defp status(102) do
    "102 Processing"
  end

  defp status(103) do
    "103 Early Hints"
  end

  defp status(200) do
    "200 OK"
  end

  defp status(201) do
    "201 Created"
  end

  defp status(202) do
    "202 Accepted"
  end

  defp status(203) do
    "203 Non-Authoritative Information"
  end

  defp status(204) do
    "204 No Content"
  end

  defp status(205) do
    "205 Reset Content"
  end

  defp status(206) do
    "206 Partial Content"
  end

  defp status(207) do
    "207 Multi-Status"
  end

  defp status(208) do
    "208 Already Reported"
  end

  defp status(226) do
    "226 IM Used"
  end

  defp status(300) do
    "300 Multiple Choices"
  end

  defp status(301) do
    "301 Moved Permanently"
  end

  defp status(302) do
    "302 Found"
  end

  defp status(303) do
    "303 See Other"
  end

  defp status(304) do
    "304 Not Modified"
  end

  defp status(305) do
    "305 Use Proxy"
  end

  defp status(306) do
    "306 Switch Proxy"
  end

  defp status(307) do
    "307 Temporary Redirect"
  end

  defp status(308) do
    "308 Permanent Redirect"
  end

  defp status(400) do
    "400 Bad Request"
  end

  defp status(401) do
    "401 Unauthorized"
  end

  defp status(402) do
    "402 Payment Required"
  end

  defp status(403) do
    "403 Forbidden"
  end

  defp status(404) do
    "404 Not Found"
  end

  defp status(405) do
    "405 Method Not Allowed"
  end

  defp status(406) do
    "406 Not Acceptable"
  end

  defp status(407) do
    "407 Proxy Authentication Required"
  end

  defp status(408) do
    "408 Request Timeout"
  end

  defp status(409) do
    "409 Conflict"
  end

  defp status(410) do
    "410 Gone"
  end

  defp status(411) do
    "411 Length Required"
  end

  defp status(412) do
    "412 Precondition Failed"
  end

  defp status(413) do
    "413 Request Entity Too Large"
  end

  defp status(414) do
    "414 Request-URI Too Long"
  end

  defp status(415) do
    "415 Unsupported Media Type"
  end

  defp status(416) do
    "416 Requested Range Not Satisfiable"
  end

  defp status(417) do
    "417 Expectation Failed"
  end

  defp status(418) do
    "418 I'm a teapot"
  end

  defp status(421) do
    "421 Misdirected Request"
  end

  defp status(422) do
    "422 Unprocessable Entity"
  end

  defp status(423) do
    "423 Locked"
  end

  defp status(424) do
    "424 Failed Dependency"
  end

  defp status(425) do
    "425 Unordered Collection"
  end

  defp status(426) do
    "426 Upgrade Required"
  end

  defp status(428) do
    "428 Precondition Required"
  end

  defp status(429) do
    "429 Too Many Requests"
  end

  defp status(431) do
    "431 Request Header Fields Too Large"
  end

  defp status(451) do
    "451 Unavailable For Legal Reasons"
  end

  defp status(500) do
    "500 Internal Server Error"
  end

  defp status(501) do
    "501 Not Implemented"
  end

  defp status(502) do
    "502 Bad Gateway"
  end

  defp status(503) do
    "503 Service Unavailable"
  end

  defp status(504) do
    "504 Gateway Timeout"
  end

  defp status(505) do
    "505 HTTP Version Not Supported"
  end

  defp status(506) do
    "506 Variant Also Negotiates"
  end

  defp status(507) do
    "507 Insufficient Storage"
  end

  defp status(508) do
    "508 Loop Detected"
  end

  defp status(510) do
    "510 Not Extended"
  end

  defp status(511) do
    "511 Network Authentication Required"
  end

  defp status(b) when is_binary(b) do
    b
  end
end
