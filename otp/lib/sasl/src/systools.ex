defmodule :m_systools do
  use Bitwise
  require Record

  Record.defrecord(:r_options, :options,
    includes: [],
    outdir: '.',
    output_type: :undefined,
    defines: [],
    warning: 1,
    verbose: false,
    optimize: 999,
    specific: [],
    outfile: '',
    cwd: :undefined
  )

  def make_script([relName | opts]) when is_atom(relName) do
    make_script([relName], opts)
  end

  def make_script(relName) do
    make_script(relName, [])
  end

  def make_script(relName, opt) do
    :systools_make.make_script(relName, opt)
  end

  def make_tar(relName) do
    make_tar(relName, [])
  end

  def make_tar(relName, opt) do
    :systools_make.make_tar(relName, opt)
  end

  def script2boot(file) do
    case :systools_lib.file_term2binary(
           file ++ '.script',
           file ++ '.boot'
         ) do
      {:error, error} ->
        :io.format('~ts', [:systools_make.format_error(error)])
        :error

      _ ->
        :ok
    end
  end

  def script2boot(file, output0, _Opt) do
    input = file ++ '.script'
    output = output0 ++ '.boot'

    case :systools_lib.file_term2binary(input, output) do
      {:error, error} ->
        :io.format('~ts', [:systools_make.format_error(error)])
        :error

      _ ->
        :ok
    end
  end

  def make_relup(releaseName, upNameList, downNameList) do
    :systools_relup.mk_relup(releaseName, upNameList, downNameList, [])
  end

  def make_relup(releaseName, upNameList, downNameList, opts) do
    :systools_relup.mk_relup(releaseName, upNameList, downNameList, opts)
  end

  def compile_rel(input, output, options) do
    :systools_make.make_script(input, output, translate_options(options))
  end

  defp translate_options(opts) do
    [{:path, r_options(opts, :includes)} | r_options(opts, :specific)]
  end
end
