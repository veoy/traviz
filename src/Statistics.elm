module Statistics exposing (transactionStddevBoundary)

import Model exposing (..)
import List exposing (foldl, head, map, range, reverse)


-- statiscs : Model -> Html Action
-- statiscs Model =
--     let
--         numberOfTransactions =
--             List.length Model.transactions |> toFloat
--
--         sumOfAbsoluteAmounts =
--             List.sum (List.map .amount Model.transactions)
--
--         sumOfAmounts =
--             List.sum (List.map amountWithSign Model.transactions)
--
--         avg =
--             sumOfAbsoluteAmounts / numberOfTransactions
--
--         stddev =
--             Basics.sqrt
--                 ((Model.transactions
--                     |> List.map (\t -> (t.amount - avg) ^ 2)
--                     |> List.sum
--                  )
--                     -- Bessel's correction
--                     / (numberOfTransactions - 1)
--                 )
--
--         asItem : String -> String -> Html Action
--         asItem title description =
--             div [ class "ui item" ]
--                 [ div [ class "ui content" ]
--                     [ div [ class "ui header" ] [ text title ]
--                     , div [ class "ui description" ] [ description |> text ]
--                     ]
--                 ]
--     in
--         div [ class "ui list" ]
--             [ (asItem "count" (toString numberOfTransactions))
--             , (asItem "sumOfAmounts" (toString sumOfAmounts))
--             , (asItem "sumOfAbsoluteAmounts" (toString sumOfAbsoluteAmounts))
--             , (asItem "averageOfAbsolutes" (toString avg))
--             , (asItem "stddevOfAbsolutes" (toString stddev))
--             ]


transactionStddevBoundary : List Transaction -> Float
transactionStddevBoundary transactions =
    let
        numberOfTransactions : Float
        numberOfTransactions =
            List.length transactions
                |> toFloat

        sumOfAbsoluteAmounts : Float
        sumOfAbsoluteAmounts =
            List.map .amount transactions
                |> List.sum

        avg =
            sumOfAbsoluteAmounts
                / numberOfTransactions

        stddev =
            Basics.sqrt
                ((transactions
                    |> List.map (\t -> (t.amount - avg) ^ 2)
                    |> List.sum
                 )
                    -- Bessel's correction
                    / (numberOfTransactions - 1)
                )
    in
        -- avg + stddev TODO: Reintroduce once we know more about what we should use
        avg + 2000
