# Copyright 2021 Manuel Montenegro
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

defmodule WhileParser.CFGTest do
  use ExUnit.Case

  import WhileParser

  test "skip statement" do
    assert {:ok,
            [
              %WhileParser.CFG.CFGBlock{
                contents: [],
                label: 1,
                succs: [:end],
                type: :skip
              }
            ], 1} = parse_to_cfg("skip")
  end

  test "two skip statements" do
    assert {:ok,
            [
              %WhileParser.CFG.CFGBlock{label: 1, succs: [2], type: :skip},
              %WhileParser.CFG.CFGBlock{label: 2, succs: [:end], type: :skip}
            ], 1} = parse_to_cfg("skip; skip")
  end

  test "three skip statements" do
    assert {:ok,
            [
              %WhileParser.CFG.CFGBlock{label: 1, succs: [2], type: :skip},
              %WhileParser.CFG.CFGBlock{label: 2, succs: [3], type: :skip},
              %WhileParser.CFG.CFGBlock{label: 3, succs: [:end], type: :skip}
            ], 1} = parse_to_cfg("skip; skip; skip")
  end

  test "statement with a single assignment" do
    assert {:ok,
            [
              %WhileParser.CFG.CFGBlock{
                contents: [
                  lhs: "x",
                  rhs:
                    {:exp, :add, 1,
                     [
                       lhs: {:exp, :variable, 1, [name: "y"]},
                       rhs: {:exp, :literal, 1, [number: 1]}
                     ]}
                ],
                label: 1,
                succs: [:end],
                type: :assignment
              }
            ], 1} = parse_to_cfg("x := y + 1")
  end

  test "skip after assignment" do
    assert {:ok,
            [
              %WhileParser.CFG.CFGBlock{label: 1, succs: [2], type: :assignment},
              %WhileParser.CFG.CFGBlock{label: 2, succs: [:end], type: :skip}
            ], 1} = parse_to_cfg("y := 3; skip")
  end

  test "conditional with if" do
    assert {:ok,
            [
              %WhileParser.CFG.CFGBlock{
                contents: [condition: {:exp, :eq, _, _}],
                label: 1,
                succs: [2, 3],
                type: :condition
              },
              %WhileParser.CFG.CFGBlock{
                contents: [
                  lhs: "x",
                  rhs: {:exp, :add, _, _}
                ],
                label: 2,
                succs: [:end],
                type: :assignment
              },
              %WhileParser.CFG.CFGBlock{
                contents: [
                  lhs: "x",
                  rhs: {:exp, :sub, _, _}
                ],
                label: 3,
                succs: [:end],
                type: :assignment
              }
            ], 1} = parse_to_cfg("if x == 0 then x := x + 1 else x := x - 1 end")
  end

  test "assignment before if" do
    assert {:ok,
            [
              %WhileParser.CFG.CFGBlock{label: 1, succs: [2], type: :assignment},
              %WhileParser.CFG.CFGBlock{label: 2, succs: [3, 4], type: :condition},
              %WhileParser.CFG.CFGBlock{label: 3, succs: [:end], type: :assignment},
              %WhileParser.CFG.CFGBlock{label: 4, succs: [:end], type: :assignment}
            ], 1} = parse_to_cfg("y := 0; if x == 0 then x := x + 1 else x := x - 1 end")
  end

  test "assignments before and after if" do
    assert {:ok,
            [
              %WhileParser.CFG.CFGBlock{label: 1, succs: [2], type: :assignment},
              %WhileParser.CFG.CFGBlock{label: 2, succs: [3, 4], type: :condition},
              %WhileParser.CFG.CFGBlock{label: 3, succs: [5], type: :assignment},
              %WhileParser.CFG.CFGBlock{label: 4, succs: [5], type: :assignment},
              %WhileParser.CFG.CFGBlock{label: 5, succs: [:end], type: :assignment}
            ], 1} = parse_to_cfg("y := 0; if x == 0 then x := x + 1 else x := x - 1 end; y := 2")
  end

  test "while loop" do
    assert {:ok,
            [
              %WhileParser.CFG.CFGBlock{
                contents: [
                  condition:
                    {:exp, :leq, 1,
                     [
                       lhs: {:exp, :variable, _, [name: "x"]},
                       rhs: {:exp, :literal, _, [number: 0]}
                     ]}
                ],
                label: 1,
                succs: [2, :end],
                type: :condition
              },
              %WhileParser.CFG.CFGBlock{
                contents: [
                  lhs: "x",
                  rhs:
                    {:exp, :add, 1,
                     [
                       lhs: {:exp, :variable, _, [name: "x"]},
                       rhs: {:exp, :literal, _, [number: 1]}
                     ]}
                ],
                label: 2,
                succs: [1],
                type: :assignment
              }
            ], 1} = parse_to_cfg("while x <= 0 do x := x + 1 end")
  end

  test "skip before while loop" do
    assert {:ok,
            [
              %WhileParser.CFG.CFGBlock{label: 1, type: :skip, succs: [2]},
              %WhileParser.CFG.CFGBlock{label: 2, type: :condition, succs: [3, :end]},
              %WhileParser.CFG.CFGBlock{label: 3, type: :assignment, succs: [2]}
            ], 1} = parse_to_cfg("skip; while x <= 0 do x := x + 1 end")
  end

  test "skip before while loop, assignment after" do
    assert {:ok,
            [
              %WhileParser.CFG.CFGBlock{label: 1, type: :skip, succs: [2]},
              %WhileParser.CFG.CFGBlock{label: 2, type: :condition, succs: [3, 4]},
              %WhileParser.CFG.CFGBlock{label: 3, type: :assignment, succs: [2]},
              %WhileParser.CFG.CFGBlock{label: 4, type: :assignment, succs: [:end]}
            ], 1} = parse_to_cfg("skip; while x <= 0 do x := x + 1 end; y := x")
  end
end
