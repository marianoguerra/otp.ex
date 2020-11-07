defmodule :m_ssh_sftpd_file do
  use Bitwise
  @behaviour :ssh_sftpd_file_api
  def close(ioDevice, state) do
    {:file.close(ioDevice), state}
  end

  def delete(path, state) do
    {:file.delete(path), state}
  end

  def del_dir(path, state) do
    {:file.del_dir(path), state}
  end

  def get_cwd(state) do
    {:file.get_cwd(), state}
  end

  def is_dir(absPath, state) do
    {:filelib.is_dir(absPath), state}
  end

  def list_dir(absPath, state) do
    {:file.list_dir(absPath), state}
  end

  def make_dir(dir, state) do
    {:file.make_dir(dir), state}
  end

  def make_symlink(path2, path, state) do
    {:file.make_symlink(path2, path), state}
  end

  def open(path, flags, state) do
    {:file.open(path, flags), state}
  end

  def position(ioDevice, offs, state) do
    {:file.position(ioDevice, offs), state}
  end

  def read(ioDevice, len, state) do
    {:file.read(ioDevice, len), state}
  end

  def read_link(path, state) do
    {:file.read_link(path), state}
  end

  def read_link_info(path, state) do
    {:file.read_link_info(path), state}
  end

  def read_file_info(path, state) do
    {:file.read_file_info(path), state}
  end

  def rename(path, path2, state) do
    {:file.rename(path, path2), state}
  end

  def write(ioDevice, data, state) do
    {:file.write(ioDevice, data), state}
  end

  def write_file_info(path, info, state) do
    {:file.write_file_info(path, info), state}
  end
end
