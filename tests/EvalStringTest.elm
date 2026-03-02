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
