module Controller exposing (..)

import Date exposing (Month(Apr))
import Date.Extra exposing (..)
import Dom exposing (..)
import Dom.Scroll as Scroll exposing (toY)
import Model exposing (..)
import Http
import Request exposing (getTransactions)
import Statistics exposing (transactionStddevBoundary)
import Task
import StaticTransactions exposing (..)


type Action
    = HandleResult (Result Dom.Error ())
    | AddDayToCurrentDate
    | RemoveDayFromCurrentDate
    | Hover Transaction
    | Unhover
    | ChangeScope TypeScope
    | StartFetchDataFromNordea
    | ReceiveDataFromService (Result Http.Error (List Transaction))


update : Action -> Model.Model -> ( Model.Model, Cmd Action )
update action model =
    let
        addDays n d =
            d
                |> add Day n
                |> Date.Extra.clamp minCurrentDate maxCurrentDate
    in
        case action of
            HandleResult result ->
                { model | message = Just (toString result) }
                    ! []

            AddDayToCurrentDate ->
                createModel
                    model.allTransactions
                    model.selectedTransaction
                    (addDays 1 model.currentDate)
                    model.typeScope
                    ! []

            RemoveDayFromCurrentDate ->
                createModel
                    model.allTransactions
                    model.selectedTransaction
                    (addDays -1 model.currentDate)
                    model.typeScope
                    ! []

            Hover t ->
                ( createModel
                    model.allTransactions
                    t
                    model.currentDate
                    model.typeScope
                , Task.attempt HandleResult <|
                    Scroll.toY "scrolling-element" (toFloat (t.rownum * 45 + 238))
                )

            Unhover ->
                createModel
                    model.allTransactions
                    voidTransaction
                    model.currentDate
                    model.typeScope
                    ! []

            ChangeScope typeScope ->
                createModel
                    model.allTransactions
                    model.selectedTransaction
                    model.currentDate
                    typeScope
                    ! []

            StartFetchDataFromNordea ->
                let
                    _ =
                        Debug.log "Test av logging" "StartFetchDataFromNordea"
                in
                    model
                        ! [ Http.send ReceiveDataFromService getTransactions ]

            ReceiveDataFromService result ->
                case result of
                    Err error ->
                        let
                            _ =
                                Debug.log "" ("myTest Failed: " ++ (toString error))
                        in
                            { model | message = Just (toString error) }
                                ! []

                    Ok transactions ->
                        let
                            _ =
                                Debug.log "" "myTest OK"

                            date =
                                case transactions of
                                    t :: _ ->
                                        t.createdDate

                                    _ ->
                                        model.currentDate
                        in
                            createModel
                                transactions
                                voidTransaction
                                date
                                model.typeScope
                                ! []


createModel : List Transaction -> Transaction -> Date.Date -> TypeScope -> Model
createModel allTransactions selectedTransaction currentDate typeScope =
    let
        lowerDateBoundary =
            currentDate
                |> Date.Extra.add Month -3
                |> Date.Extra.floor Month

        inDateBoundary : Transaction -> Bool
        inDateBoundary t =
            t |> .createdDate |> Date.Extra.isBetween lowerDateBoundary currentDate

        amountBoundary =
            transactionStddevBoundary allTransactions

        inScope : TypeScope -> Transaction -> Bool
        inScope s =
            case s of
                Big ->
                    (\t -> t.amount > amountBoundary)

                Small ->
                    (\t -> t.amount <= amountBoundary)

                All ->
                    (\t -> True)

        scopedTransactions =
            allTransactions
                |> List.filter (inScope typeScope)
                |> List.filter inDateBoundary
    in
        { message = Nothing
        , allTransactions = allTransactions
        , scopedTransactions = scopedTransactions
        , selectedTransaction = selectedTransaction
        , lowerDateBoundary = lowerDateBoundary
        , currentDate = currentDate
        , typeScope = typeScope
        }


initialModel : Model
initialModel =
    let
        addRownums i l =
            case l of
                first :: remainder ->
                    { first | rownum = i } :: addRownums (i + 1) remainder

                _ ->
                    []
    in
        -- TODO: Should this and createModel been part of model? Would naively yield cyclic dependenc
        createModel
            (addRownums 1 (merge getStaticTransactions))
            voidTransaction
            (fromParts 2017 Apr 1 0 0 0 0)
            All



-- TODO: Go to the transaction in the list via anchors
