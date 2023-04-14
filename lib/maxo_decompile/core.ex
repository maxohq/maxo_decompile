defmodule MaxoDecompile.Core do
  alias MaxoDecompile.Util
  alias MaxoDecompile.BeamFinder
  alias MaxoDecompile.AbstractCode
  alias MaxoDecompile.DebugInfo

  def run(modules, opts) do
    Enum.each(modules, &process(&1, opts))
  end

  def process(module_or_path, opts) do
    opts = Map.new(opts)
    format = Util.get_format!(opts)
    {module, data} = pure_process(module_or_path, format)

    if Map.get(opts, :stdout, true) do
      IO.puts(data)
    else
      File.write(Util.filename(module, format), data)
    end
  end

  def pure_process(module_or_path, format) do
    format = Util.map_format(format)
    BeamFinder.get!(module_or_path) |> decompile(format)
  end

  def decompile(path, format) do
    case :beam_lib.chunks(path, [:debug_info]) do
      {:ok, {module, [debug_info: {:debug_info_v1, backend, data}]}} ->
        DebugInfo.decompile(format, module, backend, data)

      {:error, :beam_lib, {:unknown_chunk, _, _}} ->
        AbstractCode.decompile(path, format)

      {:error, :beam_lib, {:missing_chunk, _, _}} ->
        AbstractCode.decompile(path, format)

      _ ->
        Mix.raise("Invalid .beam file at #{path}")
    end
  end
end
