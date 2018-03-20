module SummaryCard exposing (lastMonthsSummary, formatAsMoney, formatDate)

import BoundaryCalculations exposing (Boundary, boundaryForIndex)
import Date exposing (..)
import Date.Extra exposing (Interval(Day, Month), add, diff, fromParts)
import Element exposing (Element, column, el, row, text)
import Element.Attributes exposing (..)
import Controller exposing (..)
import Model exposing (..)
import List exposing (foldl, head, map, range, reverse)
import String exposing (padLeft)
import Stylesheet exposing (..)


lastMonthsSummary : Model -> List (Element Styles variation Controller.Action)
lastMonthsSummary em =
    [ row None
        [ spacing 10 ]
        (listMonthsSummary em)
    ]


type alias SummaryCard =
    { description : String
    , transactions : List Transaction
    , sum : Float
    , avgPositiveChange : Float
    , avgNegativeChange : Float
    , avgNetChange : Float
    }


listMonthsSummary : Model -> List (Element Styles variation Controller.Action)
listMonthsSummary em =
    let
        boundaries =
            List.range 0 2
                |> List.map (boundaryForIndex em)

        sections =
            List.map (partitionTransactionsByBoundary em.scopedTransactions) boundaries

        asMoney f =
            f
                |> round
                -- |> formatAsMoney
                |> toString
    in
        List.map
            (\s ->
                (column
                    Card
                    [ padding 20, spacing 10 ]
                    [ (el Header
                        []
                        (text s.description)
                      )
                    , (text ("∑  " ++ (asMoney s.sum)))
                    , (text ("+  " ++ (asMoney s.avgPositiveChange)))
                    , (text ("-  " ++ (asMoney -s.avgNegativeChange)))
                    , (text ("∆  " ++ (asMoney s.avgNetChange)))
                    ]
                )
            )
            sections


partitionTransactionsByBoundary : List Transaction -> Boundary -> SummaryCard
partitionTransactionsByBoundary transactions boundary =
    let
        inInterval : Boundary -> Transaction -> Bool
        inInterval boundary transaction =
            Date.Extra.isBetween boundary.from boundary.to transaction.createdDate

        transactionsInScope =
            List.filter (inInterval boundary) transactions

        asAmount =
            (\t ->
                case t.transactionType of
                    Earning ->
                        t.amount

                    Expense ->
                        -t.amount
            )

        numberOfDays =
            Date.Extra.diff Day boundary.from boundary.to
                |> toFloat

        sum =
            transactionsInScope
                |> List.map asAmount
                |> List.foldr (+) 0

        avgPositiveChange =
            transactionsInScope
                |> List.filter (\t -> t.transactionType == Earning)
                |> List.map asAmount
                |> List.foldr (+) 0
                |> (\c -> c / numberOfDays)

        avgNegativeChange =
            transactionsInScope
                |> List.filter (\t -> t.transactionType == Expense)
                |> List.map asAmount
                |> List.foldr (+) 0
                |> (\c -> c / numberOfDays)

        avgNetChange =
            sum / numberOfDays
    in
        { description = formatDate boundary.from ++ " -> " ++ formatDate boundary.to
        , transactions = transactionsInScope
        , sum = sum
        , avgPositiveChange = avgPositiveChange
        , avgNegativeChange = avgNegativeChange
        , avgNetChange = avgNetChange
        }



-- TODO: Move these next to a view utils file


formatAsMoney : Float -> String
formatAsMoney num =
    let
        above =
            num |> floor

        aboveS =
            toString above

        belowS =
            num - (toFloat above) |> (*) 100 |> round |> toString |> padLeft 2 '0'
    in
        aboveS ++ "." ++ belowS


formatDate : Date.Date -> String
formatDate date =
    let
        year =
            toString (Date.year date)

        month =
            toString (Date.month date)

        day =
            toString (Date.day date)
    in
        day ++ ". " ++ month ++ " " ++ year
