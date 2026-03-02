module EvalStringTest exposing (suite)

import Dict
import Expect exposing (Expectation)
import Stage.Eval exposing (EvalError(..), Value(..))
import Stage.Eval.ElmSyntax exposing (evalString, evalStringWithEnv)
import Test exposing (Test, describe, test)


{-| Compare Values via their string representation to avoid issues with
Elm's == operator on types containing function variants (VBuiltinFunction).
-}
expectOk : Value -> Result String Value -> Expectation
expectOk expected result =
    case result of
        Ok actual ->
            Expect.equal (valueToString expected) (valueToString actual)

        Err err ->
            Expect.fail ("Expected Ok (" ++ valueToString expected ++ ") but got Err (" ++ err ++ ")")


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

        VConstructor { module_, name, args } ->
            "VConstructor " ++ module_ ++ "." ++ name
                ++ " [" ++ String.join ", " (List.map valueToString args) ++ "]"

        VClosure _ ->
            "VClosure <closure>"

        VBuiltinFunction _ ->
            "VBuiltinFunction <function>"


suite : Test
suite =
    describe "Stage.Eval.ElmSyntax"
        [ describe "Literals"
            [ test "evaluates integer literal" <|
                \() ->
                    evalString "42"
                        |> expectOk (VInt 42)
            , test "evaluates float literal" <|
                \() ->
                    evalString "3.14"
                        |> expectOk (VFloat 3.14)
            , test "evaluates char literal" <|
                \() ->
                    evalString "'a'"
                        |> expectOk (VChar 'a')
            , test "evaluates string literal" <|
                \() ->
                    evalString "\"hello\""
                        |> expectOk (VString "hello")
            , test "evaluates unit" <|
                \() ->
                    evalString "()"
                        |> expectOk VUnit
            , test "evaluates True" <|
                \() ->
                    evalString "True"
                        |> expectOk (VBool True)
            , test "evaluates False" <|
                \() ->
                    evalString "False"
                        |> expectOk (VBool False)
            ]
        , describe "Compound expressions"
            [ test "evaluates tuple" <|
                \() ->
                    evalString "(1, 2)"
                        |> expectOk (VTuple (VInt 1) (VInt 2))
            , test "evaluates 3-tuple" <|
                \() ->
                    evalString "(1, 2, 3)"
                        |> expectOk (VTuple3 (VInt 1) (VInt 2) (VInt 3))
            , test "evaluates list" <|
                \() ->
                    evalString "[1, 2, 3]"
                        |> expectOk (VList [ VInt 1, VInt 2, VInt 3 ])
            , test "evaluates if expression" <|
                \() ->
                    evalString "if True then 1 else 2"
                        |> expectOk (VInt 1)
            , test "evaluates let expression" <|
                \() ->
                    evalString "let x = 42 in x"
                        |> expectOk (VInt 42)
            , test "evaluates lambda application" <|
                \() ->
                    evalString "(\\x -> x) 42"
                        |> expectOk (VInt 42)
            , test "evaluates record" <|
                \() ->
                    evalString "{ x = 1, y = 2 }"
                        |> expectOk (VRecord (Dict.fromList [ ( "x", VInt 1 ), ( "y", VInt 2 ) ]))
            , test "evaluates record access" <|
                \() ->
                    evalString "{ x = 1 }.x"
                        |> expectOk (VInt 1)
            , test "evaluates nested let" <|
                \() ->
                    evalString "let x = 1 in let y = 2 in x"
                        |> expectOk (VInt 1)
            , test "evaluates multi-arg lambda" <|
                \() ->
                    evalString "(\\x y -> x) 1 2"
                        |> expectOk (VInt 1)
            ]
        , describe "Operators (with builtins)"
            [ test "evaluates addition" <|
                \() ->
                    evalString "1 + 2"
                        |> expectOk (VInt 3)
            , test "evaluates subtraction" <|
                \() ->
                    evalString "5 - 3"
                        |> expectOk (VInt 2)
            , test "evaluates multiplication" <|
                \() ->
                    evalString "3 * 4"
                        |> expectOk (VInt 12)
            , test "evaluates integer division" <|
                \() ->
                    evalString "7 // 2"
                        |> expectOk (VInt 3)
            , test "evaluates float division" <|
                \() ->
                    evalString "3.0 / 2.0"
                        |> expectOk (VFloat 1.5)
            , test "evaluates negation" <|
                \() ->
                    evalString "-42"
                        |> expectOk (VInt -42)
            , test "evaluates pipe operator" <|
                \() ->
                    evalString "1 |> (\\x -> x)"
                        |> expectOk (VInt 1)
            , test "evaluates reverse pipe operator" <|
                \() ->
                    evalString "(\\x -> x) <| 1"
                        |> expectOk (VInt 1)
            ]
        , describe "Comparison operators"
            [ test "evaluates ==" <|
                \() ->
                    evalString "1 == 1"
                        |> expectOk (VBool True)
            , test "evaluates == (false)" <|
                \() ->
                    evalString "1 == 2"
                        |> expectOk (VBool False)
            , test "evaluates /=" <|
                \() ->
                    evalString "1 /= 2"
                        |> expectOk (VBool True)
            , test "evaluates <" <|
                \() ->
                    evalString "1 < 2"
                        |> expectOk (VBool True)
            , test "evaluates >" <|
                \() ->
                    evalString "2 > 1"
                        |> expectOk (VBool True)
            , test "evaluates <=" <|
                \() ->
                    evalString "1 <= 1"
                        |> expectOk (VBool True)
            , test "evaluates >=" <|
                \() ->
                    evalString "1 >= 2"
                        |> expectOk (VBool False)
            ]
        , describe "Boolean operators"
            [ test "evaluates &&" <|
                \() ->
                    evalString "True && False"
                        |> expectOk (VBool False)
            , test "evaluates ||" <|
                \() ->
                    evalString "True || False"
                        |> expectOk (VBool True)
            , test "evaluates not" <|
                \() ->
                    evalString "Basics.not True"
                        |> expectOk (VBool False)
            ]
        , describe "Case expressions"
            [ test "int literal match" <|
                \() ->
                    evalString "case 1 of\n  1 -> True\n  _ -> False"
                        |> expectOk (VBool True)
            , test "wildcard fallthrough" <|
                \() ->
                    evalString "case 2 of\n  1 -> True\n  _ -> False"
                        |> expectOk (VBool False)
            , test "var binding" <|
                \() ->
                    evalString "case 42 of\n  n -> n"
                        |> expectOk (VInt 42)
            , test "tuple destructuring" <|
                \() ->
                    evalString "case (1, 2) of\n  (a, b) -> b"
                        |> expectOk (VInt 2)
            , test "cons pattern" <|
                \() ->
                    evalString "case [1, 2, 3] of\n  x :: rest -> x\n  _ -> 0"
                        |> expectOk (VInt 1)
            , test "list pattern" <|
                \() ->
                    evalString "case [1, 2] of\n  [a, b] -> b\n  _ -> 0"
                        |> expectOk (VInt 2)
            , test "string match" <|
                \() ->
                    evalString "case \"hello\" of\n  \"hello\" -> True\n  _ -> False"
                        |> expectOk (VBool True)
            , test "record destructuring" <|
                \() ->
                    evalString "case { x = 1 } of\n  { x } -> x"
                        |> expectOk (VInt 1)
            ]
        , describe "Custom type constructors"
            [ test "Just 1 evaluates to VConstructor" <|
                \() ->
                    evalString "Just 1"
                        |> expectOk (VConstructor { module_ = "", name = "Just", args = [ VInt 1 ] })
            , test "Nothing evaluates to VConstructor" <|
                \() ->
                    evalString "Nothing"
                        |> expectOk (VConstructor { module_ = "", name = "Nothing", args = [] })
            , test "Bool constructor pattern: case True of True -> 1; False -> 0" <|
                \() ->
                    evalString "case True of\n  True -> 1\n  False -> 0"
                        |> expectOk (VInt 1)
            , test "Constructor pattern match: case Just 1 of Just n -> n; Nothing -> 0" <|
                \() ->
                    evalString "case Just 1 of\n  Just n -> n\n  Nothing -> 0"
                        |> expectOk (VInt 1)
            , test "Constructor fallthrough: case Nothing of Just n -> n; Nothing -> 0" <|
                \() ->
                    evalString "case Nothing of\n  Just n -> n\n  Nothing -> 0"
                        |> expectOk (VInt 0)
            , test "Nested constructor: case Just (Just 1) of Just (Just n) -> n; _ -> 0" <|
                \() ->
                    evalString "case Just (Just 1) of\n  Just (Just n) -> n\n  _ -> 0"
                        |> expectOk (VInt 1)
            ]
        , describe "String and List operators"
            [ test "evaluates string append" <|
                \() ->
                    evalString "\"hello\" ++ \" world\""
                        |> expectOk (VString "hello world")
            , test "evaluates list cons" <|
                \() ->
                    evalString "1 :: [2, 3]"
                        |> expectOk (VList [ VInt 1, VInt 2, VInt 3 ])
            ]
        ]
