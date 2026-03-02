module EvalTest exposing (suite)

import Dict
import Elm.AST.Typed as Typed exposing (Expr_(..), Pattern_(..))
import Elm.Data.Located as Located
import Elm.Data.Type as Type exposing (TypeOrId(..))
import Expect exposing (Expectation)
import Stage.Eval exposing (EvalError(..), Value(..), evalExpr)
import Stage.Eval.Builtins exposing (basicsEnv)
import Test exposing (Test, describe, test)


located : expr -> Located.Located expr
located expr =
    Located.located Located.dummyRegion expr


typedInt : Int -> Typed.LocatedExpr
typedInt n =
    located ( Int n, Type Type.Int )


typedString : String -> Typed.LocatedExpr
typedString s =
    located ( String s, Type Type.String )


locatedPattern : Typed.Pattern -> Typed.LocatedPattern
locatedPattern pat =
    Located.located Located.dummyRegion pat


{-| Compare Values via their string representation to avoid issues with
Elm's == operator on types containing function variants (VBuiltinFunction).
-}
expectOk : Value -> Result EvalError Value -> Expectation
expectOk expected result =
    case result of
        Ok actual ->
            Expect.equal (valueToString expected) (valueToString actual)

        Err err ->
            Expect.fail ("Expected Ok (" ++ valueToString expected ++ ") but got Err (" ++ errorToString err ++ ")")


valueToString : Value -> String
valueToString value =
    case value of
        VInt n ->
            "VInt " ++ String.fromInt n

        VFloat n ->
            "VFloat " ++ String.fromFloat n

        VChar c ->
            "VChar '" ++ String.fromChar c ++ "'"

        VString s ->
            "VString \"" ++ s ++ "\""

        VBool b ->
            "VBool "
                ++ (if b then
                        "True"

                    else
                        "False"
                   )

        VUnit ->
            "VUnit"

        VList items ->
            "VList [" ++ String.join ", " (List.map valueToString items) ++ "]"

        VTuple a b ->
            "VTuple (" ++ valueToString a ++ ") (" ++ valueToString b ++ ")"

        VTuple3 a b c ->
            "VTuple3 (" ++ valueToString a ++ ") (" ++ valueToString b ++ ") (" ++ valueToString c ++ ")"

        VRecord fields ->
            "VRecord {"
                ++ String.join ", "
                    (List.map
                        (\( k, v ) -> k ++ " = " ++ valueToString v)
                        (Dict.toList fields)
                    )
                ++ "}"

        VClosure _ ->
            "VClosure <closure>"

        VBuiltinFunction _ ->
            "VBuiltinFunction <function>"


errorToString : EvalError -> String
errorToString err =
    case err of
        TypeError msg ->
            "TypeError: " ++ msg

        VariableNotFound name ->
            "VariableNotFound: " ++ name

        FieldNotFound name ->
            "FieldNotFound: " ++ name


suite : Test
suite =
    describe "Stage.Eval"
        [ -- Cycle 1: Int literal
          describe "Int literal"
            [ test "evaluates Int 42 to VInt 42" <|
                \() ->
                    typedInt 42
                        |> evalExpr Dict.empty
                        |> expectOk (VInt 42)
            ]

        -- Cycle 2: Other literals
        , describe "Float literal"
            [ test "evaluates Float 3.14 to VFloat 3.14" <|
                \() ->
                    located ( Float 3.14, Type Type.Float )
                        |> evalExpr Dict.empty
                        |> expectOk (VFloat 3.14)
            ]
        , describe "Char literal"
            [ test "evaluates Char 'a' to VChar 'a'" <|
                \() ->
                    located ( Char 'a', Type Type.Char )
                        |> evalExpr Dict.empty
                        |> expectOk (VChar 'a')
            ]
        , describe "String literal"
            [ test "evaluates String to VString" <|
                \() ->
                    typedString "hello"
                        |> evalExpr Dict.empty
                        |> expectOk (VString "hello")
            ]
        , describe "Unit"
            [ test "evaluates Unit to VUnit" <|
                \() ->
                    located ( Unit, Type Type.Unit )
                        |> evalExpr Dict.empty
                        |> expectOk VUnit
            ]

        -- Cycle 3: Bool via ConstructorValue
        , describe "Bool via ConstructorValue"
            [ test "True evaluates to VBool True" <|
                \() ->
                    located
                        ( ConstructorValue { module_ = "Basics", name = "True" }
                        , Type Type.Bool
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VBool True)
            , test "False evaluates to VBool False" <|
                \() ->
                    located
                        ( ConstructorValue { module_ = "Basics", name = "False" }
                        , Type Type.Bool
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VBool False)
            ]

        -- Cycle 4: If expression
        , describe "If expression"
            [ test "if True then 1 else 2 -> 1" <|
                \() ->
                    located
                        ( If
                            { test =
                                located
                                    ( ConstructorValue { module_ = "Basics", name = "True" }
                                    , Type Type.Bool
                                    )
                            , then_ = typedInt 1
                            , else_ = typedInt 2
                            }
                        , Type Type.Int
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VInt 1)
            , test "if False then 1 else 2 -> 2" <|
                \() ->
                    located
                        ( If
                            { test =
                                located
                                    ( ConstructorValue { module_ = "Basics", name = "False" }
                                    , Type Type.Bool
                                    )
                            , then_ = typedInt 1
                            , else_ = typedInt 2
                            }
                        , Type Type.Int
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VInt 2)
            ]

        -- Cycle 5: Let + Argument
        , describe "Let + Argument"
            [ test "let x = 42 in x -> 42" <|
                \() ->
                    located
                        ( Let
                            { bindings =
                                Dict.singleton "x"
                                    { name = "x", body = typedInt 42 }
                            , body = located ( Argument "x", Type Type.Int )
                            }
                        , Type Type.Int
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VInt 42)
            ]

        -- Cycle 6: Lambda + Call
        , describe "Lambda + Call"
            [ test "identity function: (\\x -> x) 42 -> 42" <|
                \() ->
                    located
                        ( Call
                            { fn =
                                located
                                    ( Lambda
                                        { argument = "x"
                                        , body = located ( Argument "x", Type Type.Int )
                                        }
                                    , Type
                                        (Type.Function
                                            { from = Type Type.Int
                                            , to = Type Type.Int
                                            }
                                        )
                                    )
                            , argument = typedInt 42
                            }
                        , Type Type.Int
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VInt 42)
            ]

        -- Cycle 7: Data structures
        , describe "data structures"
            [ test "List [1, 2]" <|
                \() ->
                    located
                        ( List [ typedInt 1, typedInt 2 ]
                        , Type (Type.List (Type Type.Int))
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VList [ VInt 1, VInt 2 ])
            , test "Tuple (1, 2)" <|
                \() ->
                    located
                        ( Tuple (typedInt 1) (typedInt 2)
                        , Type (Type.Tuple (Type Type.Int) (Type Type.Int))
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VTuple (VInt 1) (VInt 2))
            , test "Tuple3 (1, 2, 3)" <|
                \() ->
                    located
                        ( Tuple3 (typedInt 1) (typedInt 2) (typedInt 3)
                        , Type
                            (Type.Tuple3
                                (Type Type.Int)
                                (Type Type.Int)
                                (Type Type.Int)
                            )
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VTuple3 (VInt 1) (VInt 2) (VInt 3))
            , test "Record { x = 1 }" <|
                \() ->
                    located
                        ( Record
                            (Dict.singleton "x"
                                { name = "x", body = typedInt 1 }
                            )
                        , Type (Type.Record (Dict.singleton "x" (Type Type.Int)))
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VRecord (Dict.singleton "x" (VInt 1)))
            ]

        -- Cycle 8: Record access
        , describe "RecordAccess"
            [ test "{ x = 1 }.x -> 1" <|
                \() ->
                    located
                        ( RecordAccess
                            (located
                                ( Record
                                    (Dict.singleton "x"
                                        { name = "x", body = typedInt 1 }
                                    )
                                , Type (Type.Record (Dict.singleton "x" (Type Type.Int)))
                                )
                            )
                            "x"
                        , Type Type.Int
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VInt 1)
            ]

        -- Cycle 9: Var lookup from pluggable environment
        , describe "Var lookup"
            [ test "looks up Var from env" <|
                \() ->
                    located
                        ( Var { module_ = "Main", name = "x" }
                        , Type Type.Int
                        )
                        |> evalExpr (Dict.singleton "Main.x" (VInt 42))
                        |> expectOk (VInt 42)
            ]

        -- Case expressions
        , describe "Case expression"
            [ test "case 1 of { 1 -> 10; _ -> 0 } evaluates to VInt 10" <|
                \() ->
                    located
                        ( Case
                            (typedInt 1)
                            ( { pattern = locatedPattern ( PInt 1, Type Type.Int )
                              , body = typedInt 10
                              }
                            , [ { pattern = locatedPattern ( PAnything, Type Type.Int )
                                , body = typedInt 0
                                }
                              ]
                            )
                        , Type Type.Int
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VInt 10)
            , test "case 2 of { 1 -> 10; _ -> 0 } falls through to wildcard" <|
                \() ->
                    located
                        ( Case
                            (typedInt 2)
                            ( { pattern = locatedPattern ( PInt 1, Type Type.Int )
                              , body = typedInt 10
                              }
                            , [ { pattern = locatedPattern ( PAnything, Type Type.Int )
                                , body = typedInt 0
                                }
                              ]
                            )
                        , Type Type.Int
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VInt 0)
            , test "case 42 of { n -> n } binds variable" <|
                \() ->
                    located
                        ( Case
                            (typedInt 42)
                            ( { pattern = locatedPattern ( PVar "n", Type Type.Int )
                              , body = located ( Argument "n", Type Type.Int )
                              }
                            , []
                            )
                        , Type Type.Int
                        )
                        |> evalExpr Dict.empty
                        |> expectOk (VInt 42)
            ]

        -- Cycle 10: Built-in arithmetic via pluggable env
        , describe "built-in arithmetic"
            [ test "1 + 2 via Basics.add in env" <|
                \() ->
                    let
                        -- Desugared form of 1 + 2:
                        -- Call (Call (Var Basics.add) 1) 2
                        expr =
                            located
                                ( Call
                                    { fn =
                                        located
                                            ( Call
                                                { fn =
                                                    located
                                                        ( Var { module_ = "Basics", name = "add" }
                                                        , Type
                                                            (Type.Function
                                                                { from = Type Type.Int
                                                                , to =
                                                                    Type
                                                                        (Type.Function
                                                                            { from = Type Type.Int
                                                                            , to = Type Type.Int
                                                                            }
                                                                        )
                                                                }
                                                            )
                                                        )
                                                , argument = typedInt 1
                                                }
                                            , Type
                                                (Type.Function
                                                    { from = Type Type.Int
                                                    , to = Type Type.Int
                                                    }
                                                )
                                            )
                                    , argument = typedInt 2
                                    }
                                , Type Type.Int
                                )
                    in
                    expr
                        |> evalExpr basicsEnv
                        |> expectOk (VInt 3)
            ]
        ]
