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
import Elm.AST.Typed as Typed exposing (Expr_(..), Pattern_(..))
import Elm.Data.Located as Located


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
    | VConstructor { module_ : String, name : String, args : List Value }
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

        Case testExpr branches ->
            evalExpr env testExpr
                |> Result.andThen
                    (\value ->
                        let
                            ( first, rest ) =
                                branches
                        in
                        matchBranches env value (first :: rest)
                    )


evalConstructor : String -> String -> Result EvalError Value
evalConstructor module_ name =
    if module_ == "Basics" && name == "True" then
        Ok (VBool True)

    else if module_ == "Basics" && name == "False" then
        Ok (VBool False)

    else
        Ok (VConstructor { module_ = module_, name = name, args = [] })


applyFunction : Value -> Value -> Result EvalError Value
applyFunction fnVal argVal =
    case fnVal of
        VClosure { argument, body, env } ->
            evalExpr (Dict.insert argument argVal env) body

        VBuiltinFunction fn ->
            fn argVal

        VConstructor { module_, name, args } ->
            Ok (VConstructor { module_ = module_, name = name, args = args ++ [ argVal ] })

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


getPattern : Typed.LocatedPattern -> Typed.Pattern_
getPattern locatedPattern =
    locatedPattern |> Located.unwrap |> Tuple.first


matchBranches : Env -> Value -> List { pattern : Typed.LocatedPattern, body : Typed.LocatedExpr } -> Result EvalError Value
matchBranches env value branches =
    case branches of
        [] ->
            Err (TypeError "No matching pattern in case expression")

        branch :: rest ->
            case matchPattern value (getPattern branch.pattern) of
                Just bindings ->
                    evalExpr (Dict.union bindings env) branch.body

                Nothing ->
                    matchBranches env value rest


matchPattern : Value -> Typed.Pattern_ -> Maybe Env
matchPattern value pattern =
    case pattern of
        PAnything ->
            Just Dict.empty

        PVar name ->
            Just (Dict.singleton name value)

        PUnit ->
            case value of
                VUnit ->
                    Just Dict.empty

                _ ->
                    Nothing

        PInt n ->
            case value of
                VInt m ->
                    if n == m then
                        Just Dict.empty

                    else
                        Nothing

                _ ->
                    Nothing

        PFloat n ->
            case value of
                VFloat m ->
                    if n == m then
                        Just Dict.empty

                    else
                        Nothing

                _ ->
                    Nothing

        PChar c ->
            case value of
                VChar d ->
                    if c == d then
                        Just Dict.empty

                    else
                        Nothing

                _ ->
                    Nothing

        PString s ->
            case value of
                VString t ->
                    if s == t then
                        Just Dict.empty

                    else
                        Nothing

                _ ->
                    Nothing

        PTuple p1 p2 ->
            case value of
                VTuple v1 v2 ->
                    Maybe.map2 Dict.union
                        (matchPattern v1 (getPattern p1))
                        (matchPattern v2 (getPattern p2))

                _ ->
                    Nothing

        PTuple3 p1 p2 p3 ->
            case value of
                VTuple3 v1 v2 v3 ->
                    Maybe.map3 (\a b c -> Dict.union a (Dict.union b c))
                        (matchPattern v1 (getPattern p1))
                        (matchPattern v2 (getPattern p2))
                        (matchPattern v3 (getPattern p3))

                _ ->
                    Nothing

        PList pats ->
            case value of
                VList vals ->
                    if List.length pats == List.length vals then
                        List.map2 (\v p -> matchPattern v (getPattern p)) vals pats
                            |> List.foldr (Maybe.map2 Dict.union) (Just Dict.empty)

                    else
                        Nothing

                _ ->
                    Nothing

        PCons hPat tPat ->
            case value of
                VList (h :: t) ->
                    Maybe.map2 Dict.union
                        (matchPattern h (getPattern hPat))
                        (matchPattern (VList t) (getPattern tPat))

                _ ->
                    Nothing

        PRecord fields ->
            case value of
                VRecord dict ->
                    fields
                        |> List.map (\field -> Dict.get field dict |> Maybe.map (\v -> ( field, v )))
                        |> List.foldr (Maybe.map2 (\( k, v ) acc -> Dict.insert k v acc)) (Just Dict.empty)

                _ ->
                    Nothing

        PAlias innerPat name ->
            matchPattern value (getPattern innerPat)
                |> Maybe.map (Dict.insert name value)

        PConstructor { name } subPatterns ->
            case value of
                VConstructor ctor ->
                    if ctor.name == name && List.length ctor.args == List.length subPatterns then
                        List.map2 (\v p -> matchPattern v (getPattern p)) ctor.args subPatterns
                            |> List.foldr (Maybe.map2 Dict.union) (Just Dict.empty)

                    else
                        Nothing

                VBool True ->
                    if name == "True" && List.isEmpty subPatterns then
                        Just Dict.empty

                    else
                        Nothing

                VBool False ->
                    if name == "False" && List.isEmpty subPatterns then
                        Just Dict.empty

                    else
                        Nothing

                _ ->
                    Nothing
