defmodule WhileParser.ParserTest do
  use ExUnit.Case

  import WhileParser.Parser

  test "parse a number" do
    assert {:ok, {:exp, :literal, 1, [number: 3]}} == parse("3")
    assert {:ok, {:exp, :literal, 2, [number: 0]}} == parse("\n0")
  end

  test "parse a negative number" do
    assert {:ok, {:exp, :literal, 1, [number: -2]}} == parse("-2")
  end

  test "parse an addition" do
    assert {:ok,
            {:exp, :add, _,
             [lhs: {:exp, :literal, _, [number: 3]}, rhs: {:exp, :literal, _, [number: 4]}]}} =
             parse("3+4")

    assert {:ok,
            {:exp, :add, _,
             [lhs: {:exp, :literal, _, [number: 3]}, rhs: {:exp, :literal, _, [number: 4]}]}} =
             parse("3 +4")

    assert {:ok,
            {:exp, :add, _,
             [lhs: {:exp, :literal, _, [number: 3]}, rhs: {:exp, :literal, _, [number: 4]}]}} =
             parse("3+ 4")

    assert {:ok,
            {:exp, :add, _,
             [lhs: {:exp, :literal, _, [number: 3]}, rhs: {:exp, :literal, _, [number: 4]}]}} =
             parse("3 + 4")

    assert {:ok,
            {:exp, :add, _,
             [lhs: {:exp, :literal, _, [number: 3]}, rhs: {:exp, :literal, _, [number: -4]}]}} =
             parse("3 + -4")

    assert {:ok,
            {:exp, :add, _,
             [lhs: {:exp, :literal, _, [number: -3]}, rhs: {:exp, :literal, _, [number: -4]}]}} =
             parse("-3 + -4")
  end

  test "parse other simple arithmetic expressions" do
    assert {:ok,
            {:exp, :sub, _,
             [lhs: {:exp, :literal, _, [number: 1]}, rhs: {:exp, :literal, _, [number: 2]}]}} =
             parse("1 - 2")

    assert {:ok,
            {:exp, :mul, _,
             [lhs: {:exp, :literal, _, [number: -1]}, rhs: {:exp, :literal, _, [number: 33]}]}} =
             parse("-1 * 33")
  end

  test "compound expression" do
    assert {:ok,
            {:exp, :sub, _,
             [
               lhs: {:exp, :literal, _, [number: 1]},
               rhs:
                 {:exp, :add, _,
                  [
                    lhs: {:exp, :literal, _, [number: 2]},
                    rhs: {:exp, :literal, _, [number: -3]}
                  ]}
             ]}} = parse("1 - (2 + -3)")
  end

  test "multilication takes more precedence than addition" do
    assert {:ok,
            {:exp, :add, _,
             [
               lhs: {:exp, :literal, _, [number: 1]},
               rhs:
                 {:exp, :mul, _,
                  [
                    lhs: {:exp, :literal, _, [number: 2]},
                    rhs: {:exp, :literal, _, [number: -3]}
                  ]}
             ]}} = parse("1 + 2 * -3")
  end

  test "changing precedence with parenthesis" do
    assert {:ok,
            {:exp, :mul, _,
             [
               lhs:
                 {:exp, :add, _,
                  [
                    lhs: {:exp, :literal, _, [number: 1]},
                    rhs: {:exp, :literal, _, [number: 2]}
                  ]},
               rhs: {:exp, :literal, _, [number: -3]}
             ]}} = parse("(1 + 2) * -3")
  end
end
