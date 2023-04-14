defmodule Mix.Tasks.Maxo.Decompile do
  alias MaxoDecompile.Core
  use Mix.Task

  def run(args) do
    {opts, modules} = OptionParser.parse!(args, strict: [to: :string, stdout: :boolean])

    # Print by default to stdout
    opts = Keyword.put_new(opts, :stdout, true)

    Mix.Task.run("loadpaths")
    Core.run(modules, opts)
  end
end
