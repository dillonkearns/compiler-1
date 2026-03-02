module OptimizeTest exposing (optimize)

import Elm.AST.Typed as Typed exposing (Expr_(..))
import Elm.Data.Type as Type exposing (TypeOrId(..))
import Expect
import Stage.Optimize
import Test exposing (Test, describe, test)
import TestHelpers
    exposing
        ( located
        , typedBool
        , typedInt
        , typedIntList
        )


optimize : Test
optimize =
    describe "Stage.Optimize"
        [ let
            runTest : ( String, Typed.LocatedExpr, Typed.LocatedExpr ) -> Test
            runTest ( description, input, output ) =
                test description <|
                    \() ->
                        input
                            |> Stage.Optimize.optimizeExpr
                            |> Expect.equal output
          in
          describe "optimizeExpr"
            [ describe "optimizeCons"
                (List.map runTest
                    [ ( "works with one value"
                      , located
                            ( Call
                                { fn =
                                    located
                                        ( Call
                                            { fn =
                                                located
                                                    ( Var { module_ = "List", name = "cons" }
                                                    , Type Type.Int
                                                    )
                                            , argument = typedInt 1
                                            }
                                        , Type Type.Int
                                        )
                                , argument = typedIntList [ 2, 3 ]
                                }
                            , Type (Type.List (Type Type.Int))
                            )
                      , typedIntList [ 1, 2, 3 ]
                      )
                    ]
                )
            , describe "optimizeIfLiteralBool"
                (List.map runTest
                    [ ( "folds to then if true"
                      , located
                            ( If
                                { test = typedBool True
                                , then_ = typedInt 42
                                , else_ = typedInt 0
                                }
                            , Type Type.Int
                            )
                      , typedInt 42
                      )
                    , ( "folds to else if false"
                      , located
                            ( If
                                { test = typedBool False
                                , then_ = typedInt 0
                                , else_ = typedInt 42
                                }
                            , Type Type.Int
                            )
                      , typedInt 42
                      )
                    , ( "doesn't work if the bool is not literal"
                      , located
                            ( If
                                { test = located ( Argument "x", Type Type.Bool )
                                , then_ = typedInt 0
                                , else_ = typedInt 42
                                }
                            , Type Type.Int
                            )
                      , located
                            ( If
                                { test = located ( Argument "x", Type Type.Bool )
                                , then_ = typedInt 0
                                , else_ = typedInt 42
                                }
                            , Type Type.Int
                            )
                      )
                    ]
                )
            ]
        ]
