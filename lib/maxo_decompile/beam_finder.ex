defmodule MaxoDecompile.BeamFinder do
  alias MaxoDecompile.Util

  def get(path) do
    get_beam!(path)
  end

  def get_beam!(module_or_path) do
    with :non_existing <- :code.which(Util.module(module_or_path)),
         :non_existing <- :code.which(String.to_atom(module_or_path)),
         :non_existing <- get_beam_file(module_or_path),
         :non_existing <- :code.where_is_file(Util.basename(module_or_path)) do
      Mix.raise("Could not find .beam file for #{module_or_path}")
    end
  end

  def get_beam_file(path) do
    list = String.to_charlist(path)

    if File.exists?(path) and not match?({:error, _, _}, :beam_lib.info(list)) do
      list
    else
      :non_existing
    end
  end
end
