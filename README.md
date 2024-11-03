## Introduction

This is a very basic type checker and inference system built for learning more
about them. I am planning to do one for the
[Vortex](https://github.com/Jintumoni/vortex) language I have been working on,
though the argument of it needing one is not strong.

I was learning Haskell for the fun of it and I thought that it would be the
perfect language for me to try this out. I was not planning to make the final
program look like a programming language, but I got carried away.

You cannot print using it, thanks to Haskell! But `GHCi` would show the result
of the final return.

## Glimpse of the "programming language"

```haskell

let addNum = Func [Var "x", Var "y"]
    [
      Assign (Var "z") (Add (Var "x") (Var "t")),
      Return (LTuple [Add (Var "z")])
    ]

let someFunc = Func []
    [
      Assign (Var "i") (LInt 10),
      Assign (Var "j") (Add (Var "i") (LInt 10)),
      Assign (LTuple [Var "add"]) (Call addNum [LInt 1, LInt 2]),
      Assign (Var "x") (Add (Var "add") (Var "j")),

      -- It has user-defined types, though I don't know what to do with them
      -- Assign (Var "w") (LComposite "Person" [
      --   ("name", LString "John"),
      --   ("age", LInt 20)
      -- ]),
      -- Get expressions allow you to extract the properties out of a user-defined type
      -- Assign (Var "a") (Get "name" (Var "w")),

      -- Array intersections
      And (LList [LInt 1, LInt 2]) (LList [LInt 1, LInt 3, LInt 2]),
      -- And union
      -- Or (LList [LInt 1, LInt 2]) (LList [LInt 1, LInt 3, LInt 2]),

      -- Function can return multiple values by default
      -- There can be multiple returns only if they are of the same type
      Return (LTuple [Var "z"])
    ]

let mainProgram = Func [] 
    [
      Return (Call someFunc [])
    ]

runProgram $ Call mainProgram []
```
