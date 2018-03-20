module View exposing (view)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave, onMouseOver)
import Controller exposing (..)
import GraphPlot exposing (graphElement)
import Model exposing (..)
import Html exposing (div, em)
import Html.Attributes exposing (sandbox, style)
import SummaryCard exposing (formatAsMoney, formatDate, lastMonthsSummary)
import Stylesheet exposing (..)


view : Model -> Html.Html Controller.Action
view em =
    Element.root Stylesheet.stylesheet <|
        column None
            []
            [ messageSection em
            , plot em
            , el None [ yScrollbar, center, width (px 800), height (px 440), id "scrolling-element" ] <|
                column None
                    [ spacing 10, paddingTop 10, paddingBottom 10 ]
                    (List.concat
                        [ controlElements em
                        , lastMonthsSummary em
                        , transactions em
                        ]
                    )
            ]


messageSection : Model -> Element Styles variation Controller.Action
messageSection em =
    case em.message of
        Just s ->
            -- empty
            html
                (div
                    [ Html.Attributes.style
                        [ ( "position", "fixed" )
                        , ( "left", "0px" )
                        , ( "bottom", "0px" )
                        , ( "z-index", "1000" )
                        , ( "background-color", "red" )
                        , ( "padding", "20px" )
                        ]
                    ]
                    [ Html.text s ]
                )

        Nothing ->
            empty


plot : Model -> Element Styles variation Controller.Action
plot em =
    row Plot
        [ padding 30, center, width (px 900), paddingBottom 0 ]
        [ html
            (div
                [ Html.Attributes.style
                    [ ( "width", "900px" )
                    ]
                ]
                [ graphElement em ]
            )
        ]


controlElements : Model -> List (Element Styles variation Controller.Action)
controlElements em =
    let
        buttonAttributes action =
            [ paddingTop 8, paddingBottom 8, paddingLeft 16, paddingRight 16, Element.Events.onClick action ]
    in
        [ row Control
            [ spacing 10 ]
            [ el Button (buttonAttributes RemoveDayFromCurrentDate) (text "- 1 Day")
            , el Button (buttonAttributes AddDayToCurrentDate) (text "+ 1 Day")
            , el Button (buttonAttributes StartFetchDataFromNordea) (text "Fetch data from Nordea")
            , comboBox
                [ ( ChangeScope All, (text "All"), All == em.typeScope )
                , ( ChangeScope Big, (text "Large"), Big == em.typeScope )
                , ( ChangeScope Small, (text "Small"), Small == em.typeScope )
                ]
            ]
        ]


comboBox : List ( Controller.Action, Element Styles variation Controller.Action, Bool ) -> Element Styles variation Controller.Action
comboBox choices =
    let
        buttonAttributes action =
            [ paddingTop 8, paddingBottom 8, paddingLeft 10, paddingRight 10, Element.Events.onClick action ]

        choiceButton ( action, face, active ) =
            let
                buttonStyle =
                    if active then
                        ActiveComboChoice
                    else
                        InactiveComboChoice
            in
                el buttonStyle (buttonAttributes action) face
    in
        row Combo
            []
            (List.map choiceButton choices)


transactions : Model -> List (Element Styles variation Controller.Action)
transactions model =
    let
        transactionRow : Transaction -> List (List (Element Styles variation Controller.Action))
        transactionRow transaction =
            let
                rowClass =
                    if transaction == model.selectedTransaction then
                        SelectedTransactionRow
                    else
                        TransactionRow

                amountClass =
                    if transaction == model.selectedTransaction then
                        SelectedTransactionRow
                    else
                        case transaction.transactionType of
                            Earning ->
                                PositiveTransactionRow

                            Expense ->
                                NegativeTransactionRow

                onClickAction =
                    if transaction == model.selectedTransaction then
                        (Unhover)
                    else
                        (Hover transaction)

                transactionRow =
                    [ el rowClass
                        [ onClick onClickAction
                        , padding 14
                        ]
                        (transaction.createdDate
                            |> formatDate
                            |> text
                        )
                    , el amountClass
                        [ onClick onClickAction
                        , padding 14
                        ]
                        (transaction.amount
                            |> formatAsMoney
                            |> text
                        )
                    ]

                transactionInfo transaction =
                    List.map
                        (\description ->
                            [ el TransactionInfoRow
                                [ onClick onClickAction
                                , padding 14
                                ]
                                (text description)
                            ]
                        )
                        transaction.description
            in
                if transaction == model.selectedTransaction then
                    List.concat [ [ transactionRow ], transactionInfo transaction ]
                else
                    [ transactionRow ]

        heading =
            [ "Date", "Amount" ]
                |> List.map text
                |> List.map (\t -> el TransactionTableHeading [ padding 14 ] t)

        toGrid rows =
            List.concat <|
                List.indexedMap
                    (\row columns ->
                        List.indexedMap
                            (\col content ->
                                if List.length columns == 2 then
                                    area
                                        { start = ( col, row ) -- Transposed relative to Element.table
                                        , width = 1
                                        , height = 1
                                        }
                                        content
                                else
                                    area
                                        { start = ( col, row ) -- Transposed relative to Element.table
                                        , width = 2
                                        , height = 1
                                        }
                                        content
                            )
                            columns
                    )
                    rows
    in
        [ Element.grid TransactionTable
            { rows = [], columns = [] }
            []
            (toGrid
                (heading
                    :: (model.scopedTransactions
                            |> List.map transactionRow
                            |> List.concat
                       )
                )
            )
        ]
