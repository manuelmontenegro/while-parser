# Copyright 2019 Manuel Montenegro
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software
# and associated documentation files (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge, publish, distribute,
# sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or substantial
# portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
# NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES
# OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
                                {:exp, :leq, 1,
                                 [
                                   lhs: {:exp, :variable, 1, [name: "y"]},
                                   rhs: {:exp, :literal, 1, [number: 10]}
                                 ]}
                            ]},
                         rhs:
                           {:exp, :leq, 1,
                            [
                              lhs: {:exp, :literal, 1, [number: 20]},
                              rhs: {:exp, :variable, 1, [name: "z"]}
                            ]}
                       ]},
                    body: [{:stm, :skip, 1, []}]
                  ]}
               ]
             ]}} == parse("while x == 0 && y <= 10 || 20 <= z do skip end")
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
                           {:exp, :leq, 1,
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

  test "tuple creation" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :assignment, 1,
                  [
                    lhs: "z",
                    rhs:
                      {:exp, :tuple, 1,
                       [
                         components: [
                           {:exp, :literal, 1, [number: 2]},
                           {:exp, :add, 1,
                            [
                              lhs: {:exp, :literal, 1, [number: 4]},
                              rhs: {:exp, :literal, 1, [number: 5]}
                            ]},
                           {:exp, :literal, 1, [number: 7]}
                         ]
                       ]}
                  ]}
               ]
             ]}} == parse("z := (2, 4+5, 7)")
  end

  test "tuple destruction" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :tuple_assignment, 1,
                  [
                    lhs: ["x", "z"],
                    rhs:
                      {:exp, :tuple, 1,
                       [
                         components: [
                           {:exp, :variable, 1, [name: "z"]},
                           {:exp, :variable, 1, [name: "x"]}
                         ]
                       ]}
                  ]}
               ]
             ]}} == parse("(x, z) := (z, x)")
  end

  test "tuple types" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :block, 1,
                  [
                    decls: [
                      {:declaration, :var_decl, 1,
                       [
                         lhs: "z",
                         rhs:
                           {:exp, :tuple, 1,
                            [
                              components: [
                                {:exp, :literal, 1, [number: 3]},
                                {:exp, :literal, 1, [boolean: true]}
                              ]
                            ]},
                         type:
                           {:type, :tuple, 1,
                            [components: [{:type, :int, 1, []}, {:type, :bool, 1, []}]]}
                       ]}
                    ],
                    body: [{:stm, :skip, 1, []}]
                  ]}
               ]
             ]}} == parse("begin var z :: (int, bool) := (3, true); skip end")
  end

  test "empty list" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [{:stm, :assignment, 1, [lhs: "x", rhs: {:exp, nil, 1, []}]}]
             ]}} == parse("x := nil")
  end

  test "singleton list" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :assignment, 1,
                  [
                    lhs: "x",
                    rhs:
                      {:exp, :cons, 1,
                       [head: {:exp, :literal, 1, [boolean: true]}, tail: {:exp, nil, 1, []}]}
                  ]}
               ]
             ]}} == parse("x := [true | nil]")
  end

  test "list of two elements" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :assignment, 1,
                  [
                    lhs: "x",
                    rhs:
                      {:exp, :cons, 1,
                       [
                         head: {:exp, :literal, 1, [number: 3]},
                         tail:
                           {:exp, :cons, 1,
                            [head: {:exp, :literal, 1, [number: 4]}, tail: {:exp, nil, 1, []}]}
                       ]}
                  ]}
               ]
             ]}} == parse("x := [3 | [4 | nil]]")
  end

  test "head of singleton list" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :assignment, 1,
                  [
                    lhs: "x",
                    rhs:
                      {:exp, :hd, 1,
                       [
                         lhs:
                           {:exp, :cons, 1,
                            [head: {:exp, :literal, 1, [number: 3]}, tail: {:exp, nil, 1, []}]}
                       ]}
                  ]}
               ]
             ]}} == parse("x := [3 | nil].hd")
  end

  test "tail of singleton list" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :assignment, 1,
                  [
                    lhs: "x",
                    rhs:
                      {:exp, :tl, 1,
                       [
                         lhs:
                           {:exp, :cons, 1,
                            [head: {:exp, :literal, 1, [boolean: true]}, tail: {:exp, nil, 1, []}]}
                       ]}
                  ]}
               ]
             ]}} == parse("x := [true | nil].tl")
  end

  test "ifnil statement" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :ifnil, 1,
                  [
                    variable: "x",
                    then: [
                      {:stm, :assignment, 1, [lhs: "z", rhs: {:exp, :literal, 1, [number: 4]}]}
                    ],
                    else: [{:stm, :skip, 1, []}, {:stm, :skip, 1, []}]
                  ]}
               ]
             ]}} == parse("ifnil x then z := 4 else skip; skip end")
  end

  test "list type" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :block, 1,
                  [
                    decls: [
                      {:declaration, :var_decl, 1,
                       [
                         lhs: "x",
                         rhs: {:exp, nil, 1, []},
                         type:
                           {:type, :list, 1,
                            [
                              elements:
                                {:type, :tuple, 1,
                                 [components: [{:type, :int, 1, []}, {:type, :bool, 1, []}]]}
                            ]}
                       ]}
                    ],
                    body: [{:stm, :skip, 1, []}]
                  ]}
               ]
             ]}} == parse("begin var x :: [(int, bool)] := nil; skip end")
  end

  test "nonempty lists" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :block, 1,
                  [
                    decls: [
                      {:declaration, :var_decl, 1,
                       [
                         lhs: "x",
                         rhs: {:exp, nil, 1, []},
                         type: {:type, :non_empty_list, 1, [elements: {:type, :bool, 1, []}]}
                       ]}
                    ],
                    body: [{:stm, :skip, 1, []}]
                  ]}
               ]
             ]}} == parse("begin var x :: [bool]+ := nil; skip end")
  end

  test "if-then-else without ambiguity" do
    assert {:ok,
            {:program, :program, 1,
             [
               functions: [],
               main_stm: [
                 {:stm, :if, 1,
                  [
                    condition:
                      {:exp, :leq, 1,
                       [
                         lhs: {:exp, :variable, 1, [name: "x"]},
                         rhs: {:exp, :literal, 1, [number: 0]}
                       ]},
                    then: [
                      {:stm, :assignment, 1, [lhs: "x", rhs: {:exp, :literal, 1, [number: 2]}]},
                      {:stm, :assignment, 1, [lhs: "y", rhs: {:exp, :literal, 1, [number: 3]}]}
                    ],
                    else: [
                      {:stm, :assignment, 1, [lhs: "y", rhs: {:exp, :literal, 1, [number: 0]}]}
                    ]
                  ]},
                 {:stm, :assignment, 1, [lhs: "z", rhs: {:exp, :variable, 1, [name: "y"]}]}
               ]
             ]}} == parse("if x <= 0 then x := 2; y := 3; else y := 0 end; z := y;")
  end

  defp unwrap_expr({:ok, {:program, :program, _, opts}}) do
    [{:stm, :assignment, _, opts_assignment} | _] = opts[:main_stm]
    {:ok, Keyword.get(opts_assignment, :rhs)}
  end
end
