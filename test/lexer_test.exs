defmodule LexerTest do
  use ExUnit.Case

  import WhileParser.Lexer

  @keywords ~w{function ret end true false in if then else while do begin}

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

  test "symbols: parentheses" do
    assert {:ok, [{:'(', _ }, {:')', _ }]} = lexer("( )")
    assert {:ok, [{:'(', _ }, {:')', _ }]} = lexer("()")
    assert {:ok, [{:')', _ }, {:'(', _ }]} = lexer(" )(")
  end

  test "some arithmetic expressions" do
    assert {:ok, [{:identifier, _, "x" }, {:'+', _ }, {:integer, _, 1}]} = lexer("x + 1")
    assert {:ok, [{:integer, _, 3 }, {:'-', _ }, {:integer, _, 5}]} = lexer("3 -5")
    assert {:ok, [{:integer, _, 3 }, {:'-', _ }, {:integer, _, 5}]} = lexer("3- 5")
    assert {:ok, [{:identifier, _, "x"}, {:'<=', _}, {:integer, _, 1}]} = lexer("x<=1")
    assert {:ok, [{:identifier, _, "x"}, {:'<=', _}, {:identifier, _, "y"}]} = lexer("x <=y")
    assert {:ok, [{:identifier, _, "x"}, {:':=', _}, {:identifier, _, "x"}, {:'+', _}, {:integer, _, 10}]} = lexer("x := x + 10")
  end

  test "separate keywords" do
    for k <- @keywords do
      expected_atom = String.to_atom(k)
      assert {:ok, [{^expected_atom, _}]} = lexer(k)
    end
  end

  test "keywords by pairs" do
    for k1 <- @keywords, k2 <- @keywords do
      expected_atom_1 = String.to_atom(k1)
      expected_atom_2 = String.to_atom(k2)
      assert {:ok, [{^expected_atom_1, _}, {^expected_atom_2, _}]} = lexer("#{k1} #{k2}")
    end
  end

  test "keywords by pairs without space" do
    for k1 <- @keywords, k2 <- @keywords do
      assert {:ok, [{:identifier, _, _}]} = lexer("#{k1}#{k2}")
    end
  end
end
