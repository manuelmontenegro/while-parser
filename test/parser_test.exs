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

  test "parse a sequence of two assignments with semicolon" do
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
             ]}} == parse("x := 3; y := x + 2;")
  end

  test "while loop with complex boolean condition" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :while, 1,
                  [
                    condition:
                      {:exp, :or, 1,
                       [
                         lhs:
                           {:exp, :and, 1,
                            [
                              lhs:
                                {:exp, :eq, 1,
                                 [
                                   lhs: {:exp, :variable, 1, [name: "x"]},
                                   rhs: {:exp, :literal, 1, [number: 0]}
                                 ]},
                              rhs:
                                {:exp, :lt, 1,
                                 [
                                   lhs: {:exp, :variable, 1, [name: "y"]},
                                   rhs: {:exp, :literal, 1, [number: 10]}
                                 ]}
                            ]},
                         rhs:
                           {:exp, :lt, 1,
                            [
                              lhs: {:exp, :literal, 1, [number: 20]},
                              rhs: {:exp, :variable, 1, [name: "z"]}
                            ]}
                       ]},
                    body: [{:stm, :skip, 1, []}]
                  ]}
               ]
             ]}} == parse("while x == 0 && y <= 10 || 20 <= z do skip")
  end

  test "conditional expression" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :assignment, 1,
                  [
                    lhs: "z",
                    rhs:
                      {:exp, :conditional, 1,
                       [
                         condition: {:exp, :literal, 1, [boolean: true]},
                         if:
                           {:exp, :add, 1,
                            [
                              lhs: {:exp, :variable, 1, [name: "x"]},
                              rhs: {:exp, :literal, 1, [number: 1]}
                            ]},
                         else:
                           {:exp, :mul, 1,
                            [
                              lhs: {:exp, :literal, 1, [number: 3]},
                              rhs: {:exp, :variable, 1, [name: "x"]}
                            ]}
                       ]}
                  ]}
               ]
             ]}} == parse("z := true ? x + 1 :  3 * x")
  end

  test "blocks with local variables" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :assignment, 1, [lhs: "z", rhs: {:exp, :literal, 1, [number: 5]}]},
                 {:stm, :block, 1,
                  [
                    decls: [
                      {:declaration, :var_decl, 1,
                       [lhs: "x", rhs: {:exp, :literal, 1, [number: 1]}]},
                      {:declaration, :var_decl, 1,
                       [
                         lhs: "y",
                         rhs:
                           {:exp, :add, 1,
                            [
                              lhs: {:exp, :variable, 1, [name: "x"]},
                              rhs: {:exp, :literal, 1, [number: 2]}
                            ]}
                       ]}
                    ],
                    body: [
                      {:stm, :assignment, 1, [lhs: "x", rhs: {:exp, :literal, 1, [number: 3]}]}
                    ]
                  ]}
               ]
             ]}} == parse("z := 5; begin var x := 1; var y:= x+2; x := 3; end")
  end

  test "blocks without local variables" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :assignment, 1, [lhs: "z", rhs: {:exp, :literal, 1, [number: 5]}]},
                 {:stm, :block, 1,
                  [
                    decls: [],
                    body: [
                      {:stm, :assignment, 1, [lhs: "x", rhs: {:exp, :literal, 1, [number: 3]}]}
                    ]
                  ]}
               ]
             ]}} == parse("z := 5; begin x := 3; end")
  end

  test "function application" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :fun_app, 1,
                  [
                    lhs: "z",
                    fun_name: "f",
                    args: [
                      {:exp, :literal, 1, [number: 3]},
                      {:exp, :add, 1,
                       [
                         lhs: {:exp, :literal, 1, [number: 5]},
                         rhs: {:exp, :variable, 1, [name: "x"]}
                       ]}
                    ]
                  ]}
               ]
             ]}} == parse("z := f(3,5 + x)")
  end

  test "function application without arguments" do
    assert {:ok,
            {:program, :program, 1,
             [functions: [], main_stm: [{:stm, :fun_app, 1, [lhs: "z", fun_name: "f", args: []]}]]}} ==
             parse("z := f()")
  end

  test "simple function declaration" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [
                 {:declaration, :fun_decl, 1,
                  [
                    function_name: "f",
                    params: [[{:declaration, :param_decl, 1, [variable: "x"]}]],
                    returns: [{:declaration, :param_decl, 1, [variable: "y"]}],
                    body: [
                      {:stm, :assignment, 1,
                       [
                         lhs: "y",
                         rhs:
                           {:exp, :add, 1,
                            [
                              lhs: {:exp, :variable, 1, [name: "x"]},
                              rhs: {:exp, :literal, 1, [number: 1]}
                            ]}
                       ]}
                    ]
                  ]}
               ],
               main_stm: [{:stm, :skip, 1, []}]
             ]}} == parse("function f(x) ret (y) y := x + 1 end skip")
  end

  test "simple function declaration with several parameters" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [
                 {:declaration, :fun_decl, 1,
                  [
                    function_name: "f",
                    params: [
                      [
                        {:declaration, :param_decl, 1,
                         [variable: "x", type: {:type, :int, 1, []}]}
                      ],
                      [
                        {:declaration, :param_decl, 1,
                         [variable: "z", type: {:type, :int, 1, []}]}
                      ]
                    ],
                    returns: [{:declaration, :param_decl, 1, [variable: "y"]}],
                    body: [
                      {:stm, :assignment, 1,
                       [
                         lhs: "y",
                         rhs:
                           {:exp, :mul, 1,
                            [
                              lhs: {:exp, :variable, 1, [name: "x"]},
                              rhs: {:exp, :variable, 1, [name: "z"]}
                            ]}
                       ]}
                    ]
                  ]}
               ],
               main_stm: [{:stm, :skip, 1, []}]
             ]}} == parse("function f(x :: int, z :: int) ret (y) y := x * z end skip")
  end

  test "simple function declaration with several parameters and type of a result" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [
                 {:declaration, :fun_decl, 1,
                  [
                    function_name: "f",
                    params: [
                      [
                        {:declaration, :param_decl, 1,
                         [variable: "x", type: {:type, :int, 1, []}]}
                      ],
                      [
                        {:declaration, :param_decl, 1,
                         [variable: "z", type: {:type, :int, 1, []}]}
                      ]
                    ],
                    returns: [
                      {:declaration, :param_decl, 1, [variable: "y", type: {:type, :bool, 1, []}]}
                    ],
                    body: [
                      {:stm, :assignment, 1,
                       [
                         lhs: "y",
                         rhs:
                           {:exp, :lt, 1,
                            [
                              lhs: {:exp, :variable, 1, [name: "x"]},
                              rhs: {:exp, :variable, 1, [name: "z"]}
                            ]}
                       ]}
                    ]
                  ]}
               ],
               main_stm: [{:stm, :skip, 1, []}]
             ]}} == parse("function f(x :: int, z :: int) ret (y :: bool) y := x <= z end skip")
  end

  test "two function declarations" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [
                 {:declaration, :fun_decl, 1,
                  [
                    function_name: "f",
                    params: [
                      [
                        {:declaration, :param_decl, 1,
                         [variable: "x", type: {:type, :int, 1, []}]}
                      ],
                      [
                        {:declaration, :param_decl, 1,
                         [variable: "z", type: {:type, :int, 1, []}]}
                      ]
                    ],
                    returns: [{:declaration, :param_decl, 1, [variable: "y"]}],
                    body: [
                      {:stm, :assignment, 1,
                       [
                         lhs: "y",
                         rhs:
                           {:exp, :mul, 1,
                            [
                              lhs: {:exp, :variable, 1, [name: "x"]},
                              rhs: {:exp, :variable, 1, [name: "z"]}
                            ]}
                       ]}
                    ]
                  ]},
                 {:declaration, :fun_decl, 1,
                  [
                    function_name: "g",
                    params: [[{:declaration, :param_decl, 1, [variable: "x"]}]],
                    returns: [{:declaration, :param_decl, 1, [variable: "z"]}],
                    body: [
                      {:stm, :assignment, 1, [lhs: "z", rhs: {:exp, :variable, 1, [name: "x"]}]}
                    ]
                  ]}
               ],
               main_stm: [
                 {:stm, :fun_app, 1,
                  [lhs: "v1", fun_name: "g", args: [{:exp, :literal, 1, [number: 3]}]]},
                 {:stm, :fun_app, 1,
                  [
                    lhs: "v2",
                    fun_name: "f",
                    args: [
                      {:exp, :variable, 1, [name: "v1"]},
                      {:exp, :literal, 1, [number: 3]}
                    ]
                  ]}
               ]
             ]}} ==
             parse(
               "function f(x :: int, z :: int) ret (y) y := x * z end function g(x) ret (z) z := x end v1 := g(3); v2 := f(v1, 3)"
             )
  end

  defp unwrap_expr({:ok, {:program, :program, _, opts}}) do
    [{:stm, :assignment, _, opts_assignment} | _] = opts[:main_stm]
    {:ok, Keyword.get(opts_assignment, :rhs)}
  end
end
