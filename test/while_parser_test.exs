defmodule WhileParserTest do
  use ExUnit.Case
  doctest WhileParser

  test "greets the world" do
    assert WhileParser.hello() == :world
  end
end
