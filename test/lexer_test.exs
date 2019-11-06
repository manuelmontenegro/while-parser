defmodule LexerTest do
  use ExUnit.Case

  import WhileParser.Lexer

  test "only whitespace" do
    assert {:ok, []} = lexer("")
    assert {:ok, []} = lexer("  \n \t")
  end

  test "simple integers" do
    assert {:ok, [{:integer, 1, 23}]} = lexer("23")
    assert {:ok, [{:integer, 2, 1}]} = lexer("\n1")
    assert {:ok, [{:integer, 1, 23}, {:integer, 3, 10}]} = lexer("23\n\n10")
  end

  test "simple identifiers" do
    assert {:ok, [{:identifier, 1, "pepe"}]} = lexer("pepe")
    assert {:ok, [{:identifier, 1, "pepe"}, {:identifier, 2, "other"}]} = lexer("pepe\nother")
    assert {:ok, [{:identifier, 1, "_ok"}]} = lexer("_ok")
    assert {:ok, [{:identifier, 1, "X1"}]} = lexer("X1")
    assert {:ok, [{:identifier, 1, "_d23"}]} = lexer("_d23")
    assert {:error, 1, _} = lexer("3d")
  end
end
