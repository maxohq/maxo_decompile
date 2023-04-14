defmodule MaxoDecompile.Util do
  @allowed_formats ["ex", "erl", "asm", "diffasm", "kernel", "core", "expanded"]
  def allowed_formats, do: @allowed_formats

  def get_format(%{to: format}), do: map_format(format)
  def get_format(_), do: Mix.raise("--to option is required (#{inspect(@allowed_formats)})")

  def map_format("ex"), do: :expanded
  def map_format("erl"), do: :erlang
  def map_format("asm"), do: :to_asm
  def map_format("diffasm"), do: :diff_asm
  def map_format("disasm"), do: :to_dis
  def map_format("kernel"), do: :to_kernel
  def map_format("core"), do: :to_core
  def map_format(other), do: String.to_atom(other)

  def ext(:to_core), do: "core"
  def ext(:to_kernel), do: "kernel"
  def ext(:to_asm), do: "S"
  def ext(other), do: other

  def module(string) do
    Module.concat(String.split(string, "."))
  end

  def basename(path) do
    String.to_charlist(Path.basename(path))
  end
end
