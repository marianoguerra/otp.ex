defmodule :m_ftp_response do
  use Bitwise

  def parse_lines(bin, lines, :start) when :erlang.size(bin) < 4 do
    {:continue, {bin, lines, :start}}
  end

  def parse_lines(<<c1, c2, c3, ?-, rest::binary>>, lines, :start) do
    parse_lines(rest, [?-, c3, c2, c1 | lines], {c1, c2, c3})
  end

  def parse_lines(<<c1, c2, c3, ?\s, bin::binary>>, lines, :start) do
    parse_lines(bin, [?\s, c3, c2, c1 | lines], :finish)
  end

  def parse_lines(<<?\r, ?\n, c1, c2, c3, ?\s, rest::binary>>, lines, {c1, c2, c3}) do
    parse_lines(rest, [?\s, c3, c2, c1, ?\n, ?\r | lines], :finish)
  end

  def parse_lines(<<?\r, ?\n, c1, c2, c3>> = bin, lines, {c1, c2, c3}) do
    {:continue, {bin, lines, {c1, c2, c3}}}
  end

  def parse_lines(<<?\r, ?\n, c1, c2, c3, rest::binary>>, lines, {c1, c2, c3}) do
    parse_lines(rest, [c3, c2, c1, ?\n, ?\r | lines], {c1, c2, c3})
  end

  def parse_lines(<<?\r, ?\n, c1, c2>> = data, lines, {c1, c2, _} = statusCode) do
    {:continue, {data, lines, statusCode}}
  end

  def parse_lines(<<?\r, ?\n, c1>> = data, lines, {c1, _, _} = statusCode) do
    {:continue, {data, lines, statusCode}}
  end

  def parse_lines(<<?\r, ?\n>> = data, lines, {_, _, _} = statusCode) do
    {:continue, {data, lines, statusCode}}
  end

  def parse_lines(<<?\n>> = data, lines, {_, _, _} = statusCode) do
    {:continue, {data, lines, statusCode}}
  end

  def parse_lines(<<>> = data, lines, {_, _, _} = statusCode) do
    {:continue, {data, lines, statusCode}}
  end

  def parse_lines(<<octet, rest::binary>>, lines, {_, _, _} = statusCode) do
    parse_lines(rest, [octet | lines], statusCode)
  end

  def parse_lines(<<?\r, ?\n>>, lines, :finish) do
    {:ok, :lists.reverse([?\n, ?\r | lines]), <<>>}
  end

  def parse_lines(<<?\r, ?\n, rest::binary>>, lines, :finish) do
    {:ok, :lists.reverse([?\n, ?\r | lines]), rest}
  end

  def parse_lines(<<?\r>> = data, lines, :finish) do
    {:continue, {data, lines, :finish}}
  end

  def parse_lines(<<>> = data, lines, :finish) do
    {:continue, {data, lines, :finish}}
  end

  def parse_lines(<<octet, rest::binary>>, lines, :finish) do
    parse_lines(rest, [octet | lines], :finish)
  end

  def interpret([didgit1, didgit2, didgit3 | data]) do
    code1 = didgit1 - ?0
    code2 = didgit2 - ?0
    code3 = didgit3 - ?0
    {interpret_status(code1, code2, code3), data}
  end

  def error_string({:error, reason}) do
    error_string(reason)
  end

  def error_string(:echunk) do
    'Synchronisation error during chunk sending.'
  end

  def error_string(:eclosed) do
    'Session has been closed.'
  end

  def error_string(:econn) do
    'Connection to remote server prematurely closed.'
  end

  def error_string(:eexists) do
    'File or directory already exists.'
  end

  def error_string(:ehost) do
    'Host not found, FTP server not found, or connection rejected.'
  end

  def error_string(:elogin) do
    'User not logged in.'
  end

  def error_string(:enotbinary) do
    'Term is not a binary.'
  end

  def error_string(:epath) do
    'No such file or directory, already exists, or permission denied.'
  end

  def error_string(:etype) do
    'No such type.'
  end

  def error_string(:euser) do
    'User name or password not valid.'
  end

  def error_string(:etnospc) do
    'Insufficient storage space in system.'
  end

  def error_string(:enofile) do
    'No files found or file unavailable'
  end

  def error_string(:epnospc) do
    'Exceeded storage allocation (for current directory or dataset).'
  end

  def error_string(:efnamena) do
    'File name not allowed.'
  end

  def error_string(reason) do
    :lists.flatten(:io_lib.format('Unknown error: ~w', [reason]))
  end

  defp interpret_status(1, _, _) do
    :pos_prel
  end

  defp interpret_status(2, 3, 4) do
    :tls_upgrade
  end

  defp interpret_status(2, _, _) do
    :pos_compl
  end

  defp interpret_status(3, 3, 2) do
    :pos_interm_acct
  end

  defp interpret_status(3, _, _) do
    :pos_interm
  end

  defp interpret_status(4, 5, 0) do
    :enofile
  end

  defp interpret_status(4, 5, 2) do
    :etnospc
  end

  defp interpret_status(4, _, _) do
    :trans_neg_compl
  end

  defp interpret_status(5, 5, 0) do
    :epath
  end

  defp interpret_status(5, 5, 2) do
    :epnospc
  end

  defp interpret_status(5, 5, 3) do
    :efnamena
  end

  defp interpret_status(5, 3, 0) do
    :elogin
  end

  defp interpret_status(5, _, _) do
    :perm_neg_compl
  end
end
