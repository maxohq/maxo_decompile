defmodule MaxoDecompileTest do
  use ExUnit.Case
  use MnemeDefaults

  test "greeting" do
    auto_assert("Welcome to Maxo!" <- MaxoDecompile.greeting())
  end
end
