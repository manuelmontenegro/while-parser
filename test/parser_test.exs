defmodule WhileParser.ParserTest do
  use ExUnit.Case

  import WhileParser.Parser

  test "parse a number" do
    assert {:ok, {:exp, :literal, 1, [number: 3]}} == parse("x := 3") |> unwrap_expr
    assert {:ok, {:exp, :literal, 2, [number: 0]}} == parse("\nx := 0") |> unwrap_expr
  end

  test "parse a negative number" do
    assert {:ok, {:exp, :literal, 1, [number: -2]}} == parse("x := -2") |> unwrap_expr
  end

  test "parse an addition" do
    assert {:ok,
            {:exp, :add, _,
             [lhs: {:exp, :literal, _, [number: 3]}, rhs: {:exp, :literal, _, [number: 4]}]}} =
             parse("x := 3+4") |> unwrap_expr

    assert {:ok,
            {:exp, :add, _,
             [lhs: {:exp, :literal, _, [number: 3]}, rhs: {:exp, :literal, _, [number: 4]}]}} =
             parse("x := 3 +4") |> unwrap_expr

    assert {:ok,
            {:exp, :add, _,
             [lhs: {:exp, :literal, _, [number: 3]}, rhs: {:exp, :literal, _, [number: 4]}]}} =
             parse("x := 3+ 4") |> unwrap_expr

    assert {:ok,
            {:exp, :add, _,
             [lhs: {:exp, :literal, _, [number: 3]}, rhs: {:exp, :literal, _, [number: 4]}]}} =
             parse("x := 3 + 4") |> unwrap_expr

    assert {:ok,
            {:exp, :add, _,
             [lhs: {:exp, :literal, _, [number: 3]}, rhs: {:exp, :literal, _, [number: -4]}]}} =
             parse("x := 3 + -4") |> unwrap_expr

    assert {:ok,
            {:exp, :add, _,
             [lhs: {:exp, :literal, _, [number: -3]}, rhs: {:exp, :literal, _, [number: -4]}]}} =
             parse("x := -3 + -4") |> unwrap_expr
  end

  test "parse other simple arithmetic expressions" do
    assert {:ok,
            {:exp, :sub, _,
             [lhs: {:exp, :literal, _, [number: 1]}, rhs: {:exp, :literal, _, [number: 2]}]}} =
             parse("x := 1 - 2") |> unwrap_expr()

    assert {:ok,
            {:exp, :mul, _,
             [lhs: {:exp, :literal, _, [number: -1]}, rhs: {:exp, :literal, _, [number: 33]}]}} =
             parse("x := -1 * 33") |> unwrap_expr()
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
             ]}} = parse("x := 1 - (2 + -3)") |> unwrap_expr()
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
             ]}} = parse("x := 1 + 2 * -3") |> unwrap_expr()
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
             ]}} = parse("x := (1 + 2) * -3") |> unwrap_expr()
  end

  test "parse simple statement: skip" do
    assert {:ok, {:program, :program, 1, [functions: [], main_stm: [{:stm, :skip, 1, []}]]}} ==
             parse("skip")
  end

  test "parse simple statement: assignment" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :assignment, 1,
                  [
                    lhs: "x",
                    rhs:
                      {:exp, :add, 1,
                       [
                         lhs: {:exp, :variable, 1, [name: "x"]},
                         rhs: {:exp, :literal, 1, [number: 4]}
                       ]}
                  ]}
               ]
             ]}} == parse("x := x + 4")
  end

  test "parse a sequence of two assignments" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :assignment, 1, [lhs: "x", rhs: {:exp, :literal, 1, [number: 3]}]},
                 {:stm, :assignment, 1,
                  [
                    lhs: "y",
                    rhs:
                      {:exp, :add, 1,
                       [
                         lhs: {:exp, :variable, 1, [name: "x"]},
                         rhs: {:exp, :literal, 1, [number: 2]}
                       ]}
                  ]}
               ]
             ]}} == parse("x := 3; y := x + 2")
  end

  defp unwrap_expr({:ok, {:program, :program, _, opts}}) do
    [{:stm, :assignment, _, opts_assignment} | _] = opts[:main_stm]
    {:ok, Keyword.get(opts_assignment, :rhs)}
  end
end
