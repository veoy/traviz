module BoundaryCalculations exposing (Boundary, boundaryForIndex)

import Array
import Date exposing (Date)
import Date.Extra exposing (..)
import Model exposing (Model, Transaction, TransactionType(Earning), minCurrentDate)
import Statistics


type alias Boundary =
    { from : Date
    , to : Date
    }


boundaryForIndex : Model -> Int -> Boundary
boundaryForIndex em i =
    let
        addDay : Boundary -> Boundary
        addDay b =
            { from = (add Day 1 b.from)
            , to = Date.Extra.clamp em.lowerDateBoundary em.currentDate (add Day 1 b.to)
            }
    in
        -- 0 is first(current), 1 is second(the preceding period), 2, is the third (twice perceding), etc
        -- asMonthBoundary em i
        asSalaryBoundary em i
            |> addDay


asMonthBoundary : Model -> Int -> Boundary
asMonthBoundary em i =
    let
        to =
            em.currentDate
                |> add Month -i

        from =
            to
                |> add Month -1
                |> add Day 1
    in
        { from = from
        , to = to
        }


asSalaryBoundary : Model -> Int -> Boundary
asSalaryBoundary em i =
    let
        -- TODO: Some duplication from the model constructor here
        lowerDateBoundary =
            em.currentDate
                |> Date.Extra.add Month -3
                |> Date.Extra.floor Month

        inDateBoundary : Transaction -> Bool
        inDateBoundary t =
            t |> .createdDate |> Date.Extra.isBetween lowerDateBoundary em.currentDate

        stddevBoundary =
            Statistics.transactionStddevBoundary em.allTransactions

        salaryDateForIndex : Int -> Date
        salaryDateForIndex i =
            em.allTransactions
                |> List.filter inDateBoundary
                |> List.filter (\t -> t.transactionType == Earning)
                |> List.filter (\t -> t.amount > stddevBoundary)
                |> List.reverse
                |> Array.fromList
                |> Array.get i
                |> Maybe.map .createdDate
                |> Maybe.withDefault Model.minCurrentDate

        from =
            salaryDateForIndex i

        to =
            case i of
                0 ->
                    em.currentDate

                _ ->
                    salaryDateForIndex (i - 1)
                        |> add Day -1
    in
        { from = from
        , to = to
        }
