defmodule MaxoDecompile.ErlangFormatter do
  alias MaxoDecompile.Util

  def do_erlang_forms(module, erlang_forms) do
    content =
      Util.string_io(fn file ->
        Enum.each(erlang_forms, &IO.puts(file, :erl_pp.form(&1)))
      end)

    {module, content}
  end
end
