module Stage.Eval.ElmSyntax exposing (evalString, evalStringWithEnv, elmSyntaxToTyped)

{-| Convert elm-syntax AST to our Typed AST, and provide a string-to-Value pipeline.

@docs evalString, evalStringWithEnv, elmSyntaxToTyped

-}

import Dict exposing (Dict)
import Elm.AST.Typed as Typed exposing (Expr_(..))
import Elm.Data.Binding exposing (Binding)
import Elm.Data.Located as Located
import Elm.Data.Type exposing (TypeOrId(..))
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as SynExpr
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as SynPat
import List.NonEmpty
import Stage.Eval exposing (Env, EvalError(..), Value, evalExpr)
import Stage.Eval.Builtins


{-| Evaluate an Elm expression string with an empty environment.
-}
evalString : String -> Result String Value
evalString exprStr =
    evalStringWithEnv Stage.Eval.Builtins.basicsEnv exprStr


{-| Evaluate an Elm expression string with a custom environment (for builtins).
-}
evalStringWithEnv : Env -> String -> Result String Value
evalStringWithEnv env exprStr =
    let
        moduleStr =
            "module Repl exposing (result)\n\nresult =\n    "
                ++ String.replace "\n" "\n    " exprStr
                ++ "\n"
    in
    case Elm.Parser.parseToFile moduleStr of
        Err deadEnds ->
            Err ("Parse error: " ++ formatDeadEnds deadEnds)

        Ok file ->
            case extractResultExpr file.declarations of
                Nothing ->
                    Err "Could not find 'result' declaration"

                Just syntaxExpr ->
                    elmSyntaxToTyped (Node.value syntaxExpr)
                        |> Result.andThen
                            (\typedExpr ->
                                evalExpr env typedExpr
                                    |> Result.mapError evalErrorToString
                            )


extractResultExpr : List (Node Declaration) -> Maybe (Node SynExpr.Expression)
extractResultExpr declarations =
    declarations
        |> List.filterMap
            (\(Node _ decl) ->
                case decl of
                    FunctionDeclaration fn ->
                        let
                            (Node _ impl) =
                                fn.declaration

                            (Node _ name) =
                                impl.name
                        in
                        if name == "result" && List.isEmpty impl.arguments then
                            Just impl.expression

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.head


formatDeadEnds : List { a | row : Int, col : Int } -> String
formatDeadEnds deadEnds =
    deadEnds
        |> List.map (\de -> "row " ++ String.fromInt de.row ++ ", col " ++ String.fromInt de.col)
        |> String.join "; "


evalErrorToString : EvalError -> String
evalErrorToString err =
    case err of
        TypeError msg ->
            "TypeError: " ++ msg

        VariableNotFound name ->
            "VariableNotFound: " ++ name

        FieldNotFound name ->
            "FieldNotFound: " ++ name



-- CONVERSION: elm-syntax Expression → Typed.LocatedExpr


dummyType : TypeOrId qualifiedness
dummyType =
    Id 0


loc : Expr_ -> Typed.LocatedExpr
loc expr =
    Located.located Located.dummyRegion ( expr, dummyType )


locPat : Typed.Pattern_ -> Typed.LocatedPattern
locPat pat =
    Located.located Located.dummyRegion ( pat, dummyType )


{-| Convert an elm-syntax Expression to our Typed.LocatedExpr.
-}
elmSyntaxToTyped : SynExpr.Expression -> Result String Typed.LocatedExpr
elmSyntaxToTyped expr =
    case expr of
        SynExpr.Integer n ->
            Ok (loc (Int n))

        SynExpr.Hex n ->
            Ok (loc (Int n))

        SynExpr.Floatable n ->
            Ok (loc (Float n))

        SynExpr.CharLiteral c ->
            Ok (loc (Char c))

        SynExpr.Literal s ->
            Ok (loc (String s))

        SynExpr.UnitExpr ->
            Ok (loc Unit)

        SynExpr.FunctionOrValue moduleName name ->
            Ok (convertFunctionOrValue moduleName name)

        SynExpr.Application nodes ->
            convertApplication nodes

        SynExpr.OperatorApplication op _ left right ->
            convertOperator op left right

        SynExpr.IfBlock test then_ else_ ->
            Result.map3
                (\t th el -> loc (If { test = t, then_ = th, else_ = el }))
                (convertNode test)
                (convertNode then_)
                (convertNode else_)

        SynExpr.LetExpression letBlock ->
            convertLet letBlock

        SynExpr.LambdaExpression lambda ->
            convertLambda lambda

        SynExpr.ListExpr items ->
            items
                |> List.map convertNode
                |> combineResults
                |> Result.map (\exprs -> loc (List exprs))

        SynExpr.TupledExpression nodes ->
            convertTuple nodes

        SynExpr.RecordExpr fields ->
            convertRecord fields

        SynExpr.RecordAccess expr_ (Node _ fieldName) ->
            convertNode expr_
                |> Result.map (\e -> loc (RecordAccess e fieldName))

        SynExpr.ParenthesizedExpression inner ->
            convertNode inner

        SynExpr.Negation inner ->
            convertNode inner
                |> Result.map
                    (\e ->
                        loc
                            (Call
                                { fn = loc (Var { module_ = "Basics", name = "negate" })
                                , argument = e
                                }
                            )
                    )

        SynExpr.CaseExpression caseBlock ->
            convertCase caseBlock

        SynExpr.PrefixOperator op ->
            operatorToFunction op
                |> Result.map (\fnRef -> loc (Var fnRef))

        SynExpr.Operator op ->
            operatorToFunction op
                |> Result.map (\fnRef -> loc (Var fnRef))

        SynExpr.RecordAccessFunction fieldName ->
            Err "Record access functions (.field) not yet supported"

        SynExpr.RecordUpdateExpression _ _ ->
            Err "Record update expressions not yet supported"

        SynExpr.GLSLExpression _ ->
            Err "GLSL expressions not supported"


convertNode : Node SynExpr.Expression -> Result String Typed.LocatedExpr
convertNode (Node _ expr) =
    elmSyntaxToTyped expr


convertFunctionOrValue : List String -> String -> Typed.LocatedExpr
convertFunctionOrValue moduleName name =
    if isUppercase name then
        let
            module_ =
                if List.isEmpty moduleName then
                    case name of
                        "True" ->
                            "Basics"

                        "False" ->
                            "Basics"

                        _ ->
                            ""

                else
                    String.join "." moduleName
        in
        loc (ConstructorValue { module_ = module_, name = name })

    else
        case moduleName of
            [] ->
                loc (Argument name)

            _ ->
                loc (Var { module_ = String.join "." moduleName, name = name })


isUppercase : String -> Bool
isUppercase s =
    case String.uncons s of
        Just ( c, _ ) ->
            Char.isUpper c

        Nothing ->
            False


convertApplication : List (Node SynExpr.Expression) -> Result String Typed.LocatedExpr
convertApplication nodes =
    case nodes of
        [] ->
            Err "Empty application"

        [ single ] ->
            convertNode single

        fn :: args ->
            convertNode fn
                |> Result.andThen
                    (\fnExpr ->
                        List.foldl
                            (\argNode accResult ->
                                accResult
                                    |> Result.andThen
                                        (\acc ->
                                            convertNode argNode
                                                |> Result.map
                                                    (\arg ->
                                                        loc (Call { fn = acc, argument = arg })
                                                    )
                                        )
                            )
                            (Ok fnExpr)
                            args
                    )


operatorToFunction : String -> Result String { module_ : String, name : String }
operatorToFunction op =
    case op of
        "+" ->
            Ok { module_ = "Basics", name = "add" }

        "-" ->
            Ok { module_ = "Basics", name = "sub" }

        "*" ->
            Ok { module_ = "Basics", name = "mul" }

        "/" ->
            Ok { module_ = "Basics", name = "fdiv" }

        "//" ->
            Ok { module_ = "Basics", name = "idiv" }

        "==" ->
            Ok { module_ = "Basics", name = "eq" }

        "/=" ->
            Ok { module_ = "Basics", name = "neq" }

        "<" ->
            Ok { module_ = "Basics", name = "lt" }

        ">" ->
            Ok { module_ = "Basics", name = "gt" }

        "<=" ->
            Ok { module_ = "Basics", name = "le" }

        ">=" ->
            Ok { module_ = "Basics", name = "ge" }

        "&&" ->
            Ok { module_ = "Basics", name = "and" }

        "||" ->
            Ok { module_ = "Basics", name = "or" }

        "++" ->
            Ok { module_ = "Basics", name = "append" }

        "::" ->
            Ok { module_ = "List", name = "cons" }

        _ ->
            Err ("Unknown operator: " ++ op)


convertOperator : String -> Node SynExpr.Expression -> Node SynExpr.Expression -> Result String Typed.LocatedExpr
convertOperator op left right =
    case op of
        "|>" ->
            Result.map2
                (\l r -> loc (Call { fn = r, argument = l }))
                (convertNode left)
                (convertNode right)

        "<|" ->
            Result.map2
                (\l r -> loc (Call { fn = l, argument = r }))
                (convertNode left)
                (convertNode right)

        _ ->
            operatorToFunction op
                |> Result.andThen
                    (\fnRef ->
                        Result.map2
                            (\l r ->
                                loc
                                    (Call
                                        { fn =
                                            loc
                                                (Call
                                                    { fn = loc (Var fnRef)
                                                    , argument = l
                                                    }
                                                )
                                        , argument = r
                                        }
                                    )
                            )
                            (convertNode left)
                            (convertNode right)
                    )


convertLet : SynExpr.LetBlock -> Result String Typed.LocatedExpr
convertLet { declarations, expression } =
    let
        convertDecl : Node SynExpr.LetDeclaration -> Result String ( String, Binding Typed.LocatedExpr )
        convertDecl (Node _ decl) =
            case decl of
                SynExpr.LetFunction fn ->
                    let
                        (Node _ impl) =
                            fn.declaration

                        (Node _ name) =
                            impl.name
                    in
                    if List.isEmpty impl.arguments then
                        convertNode impl.expression
                            |> Result.map (\body -> ( name, { name = name, body = body } ))

                    else
                        convertFunctionArgs impl.arguments impl.expression
                            |> Result.map (\body -> ( name, { name = name, body = body } ))

                SynExpr.LetDestructuring _ _ ->
                    Err "Let destructuring patterns not yet supported"
    in
    declarations
        |> List.map convertDecl
        |> combineResults
        |> Result.andThen
            (\bindings ->
                convertNode expression
                    |> Result.map
                        (\body ->
                            loc
                                (Let
                                    { bindings = Dict.fromList bindings
                                    , body = body
                                    }
                                )
                        )
            )


convertFunctionArgs : List (Node SynPat.Pattern) -> Node SynExpr.Expression -> Result String Typed.LocatedExpr
convertFunctionArgs args exprNode =
    convertNode exprNode
        |> Result.andThen
            (\body ->
                List.foldr
                    (\(Node _ pat) accResult ->
                        accResult
                            |> Result.andThen
                                (\acc ->
                                    case pat of
                                        SynPat.VarPattern name ->
                                            Ok (loc (Lambda { argument = name, body = acc }))

                                        _ ->
                                            Err "Only simple variable patterns supported in function arguments"
                                )
                    )
                    (Ok body)
                    args
            )


convertLambda : SynExpr.Lambda -> Result String Typed.LocatedExpr
convertLambda { args, expression } =
    convertNode expression
        |> Result.andThen
            (\body ->
                List.foldr
                    (\(Node _ pat) accResult ->
                        accResult
                            |> Result.andThen
                                (\acc ->
                                    case pat of
                                        SynPat.VarPattern name ->
                                            Ok (loc (Lambda { argument = name, body = acc }))

                                        _ ->
                                            Err "Only simple variable patterns supported in lambda arguments"
                                )
                    )
                    (Ok body)
                    args
            )


convertTuple : List (Node SynExpr.Expression) -> Result String Typed.LocatedExpr
convertTuple nodes =
    case nodes of
        [] ->
            Ok (loc Unit)

        [ single ] ->
            convertNode single

        [ a, b ] ->
            Result.map2
                (\ea eb -> loc (Tuple ea eb))
                (convertNode a)
                (convertNode b)

        [ a, b, c ] ->
            Result.map3
                (\ea eb ec -> loc (Tuple3 ea eb ec))
                (convertNode a)
                (convertNode b)
                (convertNode c)

        _ ->
            Err "Tuples with more than 3 elements are not supported"


convertRecord : List (Node SynExpr.RecordSetter) -> Result String Typed.LocatedExpr
convertRecord fields =
    fields
        |> List.map
            (\(Node _ ( Node _ name, valueNode )) ->
                convertNode valueNode
                    |> Result.map (\v -> ( name, { name = name, body = v } ))
            )
        |> combineResults
        |> Result.map (\pairs -> loc (Record (Dict.fromList pairs)))


convertCase : SynExpr.CaseBlock -> Result String Typed.LocatedExpr
convertCase { expression, cases } =
    case cases of
        [] ->
            Err "Case expression with no branches"

        first :: rest ->
            convertNode expression
                |> Result.andThen
                    (\testExpr ->
                        convertCaseBranch first
                            |> Result.andThen
                                (\firstBranch ->
                                    rest
                                        |> List.map convertCaseBranch
                                        |> combineResults
                                        |> Result.map
                                            (\restBranches ->
                                                loc (Case testExpr ( firstBranch, restBranches ))
                                            )
                                )
                    )


convertCaseBranch : SynExpr.Case -> Result String { pattern : Typed.LocatedPattern, body : Typed.LocatedExpr }
convertCaseBranch ( patNode, exprNode ) =
    Result.map2
        (\pat body -> { pattern = pat, body = body })
        (convertPattern patNode)
        (convertNode exprNode)


convertPattern : Node SynPat.Pattern -> Result String Typed.LocatedPattern
convertPattern (Node _ pat) =
    case pat of
        SynPat.AllPattern ->
            Ok (locPat Typed.PAnything)

        SynPat.VarPattern name ->
            Ok (locPat (Typed.PVar name))

        SynPat.UnitPattern ->
            Ok (locPat Typed.PUnit)

        SynPat.CharPattern c ->
            Ok (locPat (Typed.PChar c))

        SynPat.StringPattern s ->
            Ok (locPat (Typed.PString s))

        SynPat.IntPattern n ->
            Ok (locPat (Typed.PInt n))

        SynPat.HexPattern n ->
            Ok (locPat (Typed.PInt n))

        SynPat.FloatPattern n ->
            Ok (locPat (Typed.PFloat n))

        SynPat.TuplePattern nodes ->
            case nodes of
                [ a, b ] ->
                    Result.map2
                        (\pa pb -> locPat (Typed.PTuple pa pb))
                        (convertPattern a)
                        (convertPattern b)

                [ a, b, c ] ->
                    Result.map3
                        (\pa pb pc -> locPat (Typed.PTuple3 pa pb pc))
                        (convertPattern a)
                        (convertPattern b)
                        (convertPattern c)

                _ ->
                    Err "Unsupported tuple pattern"

        SynPat.ListPattern nodes ->
            nodes
                |> List.map convertPattern
                |> combineResults
                |> Result.map (\pats -> locPat (Typed.PList pats))

        SynPat.UnConsPattern head tail ->
            Result.map2
                (\h t -> locPat (Typed.PCons h t))
                (convertPattern head)
                (convertPattern tail)

        SynPat.RecordPattern fields ->
            Ok (locPat (Typed.PRecord (List.map Node.value fields)))

        SynPat.AsPattern inner (Node _ name) ->
            convertPattern inner
                |> Result.map (\p -> locPat (Typed.PAlias p name))

        SynPat.ParenthesizedPattern inner ->
            convertPattern inner

        SynPat.NamedPattern _ _ ->
            Err "Constructor patterns not yet supported"


combineResults : List (Result x a) -> Result x (List a)
combineResults =
    List.foldr (Result.map2 (::)) (Ok [])
