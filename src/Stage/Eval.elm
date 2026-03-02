module Stage.Eval exposing
    ( Value(..)
    , EvalError(..)
    , Env
    , evalExpr
    )

{-| Interpreter/eval stage for the elm-in-elm compiler.

Takes a `Typed.LocatedExpr` (already type-checked) and produces a runtime `Value`.

@docs Value, EvalError, Env, evalExpr

-}

import Dict exposing (Dict)
import Elm.AST.Typed as Typed exposing (Expr_(..))


{-| Runtime environment mapping variable names to values.
Keys are qualified names like "Basics.add" for module-level vars,
or simple names like "x" for local bindings (from Let/Lambda).
-}
type alias Env =
    Dict String Value


{-| Runtime values produced by evaluation.
-}
type Value
    = VInt Int
    | VFloat Float
    | VChar Char
    | VString String
    | VBool Bool
    | VUnit
    | VList (List Value)
    | VTuple Value Value
    | VTuple3 Value Value Value
    | VRecord (Dict String Value)
    | VClosure { argument : String, body : Typed.LocatedExpr, env : Env }
    | VBuiltinFunction (Value -> Result EvalError Value)


{-| Errors that can occur during evaluation.
-}
type EvalError
    = TypeError String
    | VariableNotFound String
    | FieldNotFound String


{-| Evaluate a typed expression in the given environment.
-}
evalExpr : Env -> Typed.LocatedExpr -> Result EvalError Value
evalExpr env locatedExpr =
    case Typed.getExpr locatedExpr of
        Int n ->
            Ok (VInt n)

        Float n ->
            Ok (VFloat n)

        Char c ->
            Ok (VChar c)

        String s ->
            Ok (VString s)

        Unit ->
            Ok VUnit

        ConstructorValue { module_, name } ->
            evalConstructor module_ name

        If { test, then_, else_ } ->
            evalExpr env test
                |> Result.andThen
                    (\testVal ->
                        case testVal of
                            VBool True ->
                                evalExpr env then_

                            VBool False ->
                                evalExpr env else_

                            _ ->
                                Err (TypeError "If condition must be a Bool")
                    )

        Argument varName ->
            case Dict.get varName env of
                Just val ->
                    Ok val

                Nothing ->
                    Err (VariableNotFound varName)

        Let { bindings, body } ->
            bindings
                |> Dict.toList
                |> List.map (\( key, binding ) -> ( key, binding.body ))
                |> evalBindings env
                |> Result.andThen (\newEnv -> evalExpr newEnv body)

        Lambda { argument, body } ->
            Ok (VClosure { argument = argument, body = body, env = env })

        Call { fn, argument } ->
            evalExpr env fn
                |> Result.andThen
                    (\fnVal ->
                        evalExpr env argument
                            |> Result.andThen (applyFunction fnVal)
                    )

        List items ->
            items
                |> List.map (evalExpr env)
                |> combineResults
                |> Result.map VList

        Tuple e1 e2 ->
            Result.map2 VTuple
                (evalExpr env e1)
                (evalExpr env e2)

        Tuple3 e1 e2 e3 ->
            Result.map3 VTuple3
                (evalExpr env e1)
                (evalExpr env e2)
                (evalExpr env e3)

        Record bindings ->
            bindings
                |> Dict.map (\_ binding -> evalExpr env binding.body)
                |> combineDict
                |> Result.map VRecord

        RecordAccess recordExpr fieldName ->
            evalExpr env recordExpr
                |> Result.andThen
                    (\recordVal ->
                        case recordVal of
                            VRecord fields ->
                                case Dict.get fieldName fields of
                                    Just val ->
                                        Ok val

                                    Nothing ->
                                        Err (FieldNotFound fieldName)

                            _ ->
                                Err (TypeError "RecordAccess on non-record")
                    )

        Var { module_, name } ->
            let
                qualifiedName =
                    module_ ++ "." ++ name
            in
            case Dict.get qualifiedName env of
                Just val ->
                    Ok val

                Nothing ->
                    Err (VariableNotFound qualifiedName)

        Case _ _ ->
            Err (TypeError "Case expressions not yet supported")


evalConstructor : String -> String -> Result EvalError Value
evalConstructor module_ name =
    if module_ == "Basics" && name == "True" then
        Ok (VBool True)

    else if module_ == "Basics" && name == "False" then
        Ok (VBool False)

    else
        Err (TypeError ("Unknown constructor: " ++ module_ ++ "." ++ name))


applyFunction : Value -> Value -> Result EvalError Value
applyFunction fnVal argVal =
    case fnVal of
        VClosure { argument, body, env } ->
            evalExpr (Dict.insert argument argVal env) body

        VBuiltinFunction fn ->
            fn argVal

        _ ->
            Err (TypeError "Attempted to call a non-function")


evalBindings : Env -> List ( String, Typed.LocatedExpr ) -> Result EvalError Env
evalBindings env bindings =
    case bindings of
        [] ->
            Ok env

        ( key, bindingBody ) :: rest ->
            evalExpr env bindingBody
                |> Result.andThen
                    (\val ->
                        evalBindings (Dict.insert key val env) rest
                    )


combineResults : List (Result x a) -> Result x (List a)
combineResults =
    List.foldr (Result.map2 (::)) (Ok [])


combineDict : Dict String (Result x a) -> Result x (Dict String a)
combineDict dict =
    dict
        |> Dict.toList
        |> List.foldr
            (\( key, result ) acc ->
                Result.map2
                    (\val accDict -> Dict.insert key val accDict)
                    result
                    acc
            )
            (Ok Dict.empty)
