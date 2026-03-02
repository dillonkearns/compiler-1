module Stage.Eval.Builtins exposing (basicsEnv)

{-| Shared built-in environment for the Elm evaluator.

Provides `basicsEnv`, a `Dict String Value` keyed by qualified names
(e.g. "Basics.add") containing curried implementations of standard Elm
functions that operators desugar to.

@docs basicsEnv

-}

import Dict
import Stage.Eval exposing (Env, EvalError(..), Value(..))


basicsEnv : Env
basicsEnv =
    Dict.fromList
        [ -- Arithmetic (Int)
          ( "Basics.add", intBinOp (+) )
        , ( "Basics.sub", intBinOp (-) )
        , ( "Basics.mul", intBinOp (*) )
        , ( "Basics.idiv", intBinOp (//) )
        , ( "Basics.modBy", intBinOp modBy )
        , ( "Basics.negate"
          , VBuiltinFunction
                (\v ->
                    case v of
                        VInt n ->
                            Ok (VInt (negate n))

                        VFloat n ->
                            Ok (VFloat (negate n))

                        _ ->
                            Err (TypeError "negate expects a number")
                )
          )
        , ( "Basics.abs"
          , VBuiltinFunction
                (\v ->
                    case v of
                        VInt n ->
                            Ok (VInt (abs n))

                        VFloat n ->
                            Ok (VFloat (abs n))

                        _ ->
                            Err (TypeError "abs expects a number")
                )
          )

        -- Arithmetic (Float)
        , ( "Basics.fdiv", floatBinOp (/) )
        , ( "Basics.toFloat"
          , VBuiltinFunction
                (\v ->
                    case v of
                        VInt n ->
                            Ok (VFloat (toFloat n))

                        _ ->
                            Err (TypeError "toFloat expects an Int")
                )
          )
        , ( "Basics.round"
          , VBuiltinFunction
                (\v ->
                    case v of
                        VFloat n ->
                            Ok (VInt (round n))

                        _ ->
                            Err (TypeError "round expects a Float")
                )
          )
        , ( "Basics.floor"
          , VBuiltinFunction
                (\v ->
                    case v of
                        VFloat n ->
                            Ok (VInt (floor n))

                        _ ->
                            Err (TypeError "floor expects a Float")
                )
          )
        , ( "Basics.ceiling"
          , VBuiltinFunction
                (\v ->
                    case v of
                        VFloat n ->
                            Ok (VInt (ceiling n))

                        _ ->
                            Err (TypeError "ceiling expects a Float")
                )
          )
        , ( "Basics.truncate"
          , VBuiltinFunction
                (\v ->
                    case v of
                        VFloat n ->
                            Ok (VInt (truncate n))

                        _ ->
                            Err (TypeError "truncate expects a Float")
                )
          )

        -- Comparison
        , ( "Basics.eq", comparisonOp (\a b -> compareValues a b == Just EQ) )
        , ( "Basics.neq", comparisonOp (\a b -> compareValues a b /= Just EQ) )
        , ( "Basics.lt", comparisonOp (\a b -> compareValues a b == Just LT) )
        , ( "Basics.gt", comparisonOp (\a b -> compareValues a b == Just GT) )
        , ( "Basics.le", comparisonOp (\a b -> compareValues a b /= Just GT) )
        , ( "Basics.ge", comparisonOp (\a b -> compareValues a b /= Just LT) )

        -- Boolean
        , ( "Basics.and", boolBinOp (&&) )
        , ( "Basics.or", boolBinOp (||) )
        , ( "Basics.not"
          , VBuiltinFunction
                (\v ->
                    case v of
                        VBool b ->
                            Ok (VBool (not b))

                        _ ->
                            Err (TypeError "not expects a Bool")
                )
          )

        -- String / List append
        , ( "Basics.append"
          , VBuiltinFunction
                (\a ->
                    Ok
                        (VBuiltinFunction
                            (\b ->
                                case ( a, b ) of
                                    ( VString x, VString y ) ->
                                        Ok (VString (x ++ y))

                                    ( VList x, VList y ) ->
                                        Ok (VList (x ++ y))

                                    _ ->
                                        Err (TypeError "append expects two Strings or two Lists")
                            )
                        )
                )
          )

        -- List.cons
        , ( "List.cons"
          , VBuiltinFunction
                (\head ->
                    Ok
                        (VBuiltinFunction
                            (\tail ->
                                case tail of
                                    VList items ->
                                        Ok (VList (head :: items))

                                    _ ->
                                        Err (TypeError "cons expects a List as second argument")
                            )
                        )
                )
          )
        ]



-- HELPERS


intBinOp : (Int -> Int -> Int) -> Value
intBinOp op =
    VBuiltinFunction
        (\a ->
            case a of
                VInt x ->
                    Ok
                        (VBuiltinFunction
                            (\b ->
                                case b of
                                    VInt y ->
                                        Ok (VInt (op x y))

                                    _ ->
                                        Err (TypeError "Expected Int")
                            )
                        )

                _ ->
                    Err (TypeError "Expected Int")
        )


floatBinOp : (Float -> Float -> Float) -> Value
floatBinOp op =
    VBuiltinFunction
        (\a ->
            case a of
                VFloat x ->
                    Ok
                        (VBuiltinFunction
                            (\b ->
                                case b of
                                    VFloat y ->
                                        Ok (VFloat (op x y))

                                    _ ->
                                        Err (TypeError "Expected Float")
                            )
                        )

                _ ->
                    Err (TypeError "Expected Float")
        )


boolBinOp : (Bool -> Bool -> Bool) -> Value
boolBinOp op =
    VBuiltinFunction
        (\a ->
            case a of
                VBool x ->
                    Ok
                        (VBuiltinFunction
                            (\b ->
                                case b of
                                    VBool y ->
                                        Ok (VBool (op x y))

                                    _ ->
                                        Err (TypeError "Expected Bool")
                            )
                        )

                _ ->
                    Err (TypeError "Expected Bool")
        )


comparisonOp : (Value -> Value -> Bool) -> Value
comparisonOp pred =
    VBuiltinFunction
        (\a ->
            Ok
                (VBuiltinFunction
                    (\b ->
                        Ok (VBool (pred a b))
                    )
                )
        )


compareValues : Value -> Value -> Maybe Order
compareValues a b =
    case ( a, b ) of
        ( VInt x, VInt y ) ->
            Just (compare x y)

        ( VFloat x, VFloat y ) ->
            Just (compare x y)

        ( VChar x, VChar y ) ->
            Just (compare x y)

        ( VString x, VString y ) ->
            Just (compare x y)

        ( VInt x, VFloat y ) ->
            Just (compare (toFloat x) y)

        ( VFloat x, VInt y ) ->
            Just (compare x (toFloat y))

        ( VBool x, VBool y ) ->
            let
                boolToInt bool =
                    if bool then
                        1

                    else
                        0
            in
            Just (compare (boolToInt x) (boolToInt y))

        _ ->
            Nothing
