defmodule MaxoDecompile.ErlangFormatter do
  @doc """

  - https://www.erlang.org/doc/man/io.html#standard-input-output
    This is the case with functions that can access either a file or the default I/O device.
    The atom :standard_io has this special meaning. The following example illustrates this
  """
  def do_erlang_forms(module, erlang_forms) do
    # File.open("#{module}.erl", [:write], fn file ->
    # Enum.each(erlang_forms, &IO.puts(file, :erl_pp.form(&1)))

    {:ok, file} = StringIO.open("#{module}.erl")
    # Enum.each(erlang_forms, &IO.puts(:standard_io, :erl_pp.form(&1)))
    Enum.each(erlang_forms, &IO.puts(file, :erl_pp.form(&1)))

    {_fname, content} = StringIO.contents(file)
    {module, content}
  end
end
