module Model exposing (..)

import Date exposing (Date, Month(..), fromString, toTime)
import Date.Extra exposing (fromParts)
import List exposing (sortBy)


type TypeScope
    = Big
    | Small
    | All


type alias Model =
    { message : Maybe String
    , allTransactions : List Transaction
    , scopedTransactions : List Transaction
    , selectedTransaction : Transaction
    , lowerDateBoundary : Date
    , currentDate : Date
    , typeScope : TypeScope
    }


maxCurrentDate : Date
maxCurrentDate =
    fromParts 2017 May 16 0 0 0 0


minCurrentDate : Date
minCurrentDate =
    fromParts 2017 Apr 1 0 0 0 0


voidTransaction : Transaction
voidTransaction =
    createTransaction ( "2000.01.01", "2000.01.01", "", 0.0, Expense )


type alias Transaction =
    { createdDate : Date
    , interestDate : Date
    , transactionType : TransactionType
    , amount : Float
    , description : List String
    , rownum : Int
    }


type TransactionType
    = Earning
    | Expense


myDateFromString : String -> Date
myDateFromString s =
    let
        r =
            Date.fromString s
    in
        case r of
            Ok v ->
                v

            Err e ->
                Date.fromTime 1


merge : List Transaction -> List Transaction
merge l =
    let
        mergeAdjacentElementsWithEqualDate : List Transaction -> List Transaction
        mergeAdjacentElementsWithEqualDate l =
            case l of
                first :: second :: remainder ->
                    if first.createdDate == second.createdDate then
                        mergeAdjacentElementsWithEqualDate (mergeTransactions first second :: remainder)
                    else
                        first :: mergeAdjacentElementsWithEqualDate (second :: remainder)

                remainder ->
                    remainder
    in
        sortBy (\t -> t.createdDate |> toTime) l |> mergeAdjacentElementsWithEqualDate


amountWithSign : Transaction -> Float
amountWithSign t =
    case t.transactionType of
        Earning ->
            t.amount

        Expense ->
            -t.amount


mergeTransactions : Transaction -> Transaction -> Transaction
mergeTransactions l1 l2 =
    let
        amount =
            (amountWithSign l1) + (amountWithSign l2)

        transactionType =
            if amount > 0 then
                Earning
            else
                Expense

        absoluteAmount =
            if amount > 0 then
                amount
            else
                -amount
    in
        { createdDate = l1.createdDate
        , interestDate = l1.interestDate
        , transactionType = transactionType
        , amount = absoluteAmount
        , description = List.concat [ l1.description, l2.description ]
        , rownum = 0
        }


createTransaction : ( String, String, String, Float, TransactionType ) -> Transaction
createTransaction ( createdDate, interestDate, description, amount, transactionType ) =
    { createdDate = myDateFromString createdDate
    , interestDate = myDateFromString interestDate
    , transactionType = transactionType
    , amount = amount
    , description = [ description ]
    , rownum = 0
    }
