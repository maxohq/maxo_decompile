defmodule MaxoDecompile.ErlangFormatter do
  def format_erlang_forms(_module, erlang_forms) do
    # File.open("#{module}.erl", [:write], fn file ->
    # Enum.each(erlang_forms, &IO.puts(file, :erl_pp.form(&1)))
    Enum.each(erlang_forms, &IO.puts(:standard_io, :erl_pp.form(&1)))
    # end)
  end
end
