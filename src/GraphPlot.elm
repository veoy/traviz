module GraphPlot exposing (graphElement)

import BoundaryCalculations exposing (boundaryForIndex)
import Date exposing (..)
import Date.Extra
import Controller exposing (..)
import Model exposing (..)
import Html.Attributes exposing (style)
import List exposing (head, map, range, reverse)
import Plot exposing (..)
import Statistics
import Svg exposing (Svg)
import Svg.Attributes exposing (fill, opacity, r, stroke, x)
import Svg.Events exposing (onClick)


graphElement : Model -> Svg Action
graphElement em =
    viewSeriesCustom
        { defaultSeriesPlotCustomizations
            | grid = { vertical = clearGrid, horizontal = clearGrid }
            , height = 250
            , horizontalAxis = horizontalAxis em
            , margin =
                { top = 30
                , right = 50
                , bottom = 30
                , left = 10 -- 60 to see labels
                }
            , junk =
                \summary -> [ junk title summary.x.dataMax summary.y.max ]
            , toDomainLowest = \y -> y
            , toRangeLowest = \y -> y
        }
        [ visualizationForMonth "rgb(11,72,107)" 2

        -- TODO: Weird bug if we move this line above the previous: The onhover does not work...
        , visualizationForMonth "rgb(11,72,107)" 1
        , visualizationForMonth "rgb(11,72,107)" 0
        ]
        em



-- Colors used to be #0B486B, #3B8686, #79BD9A
-- Colors used to be rgb(11,72,107), rgb(133,163,181), rgb(194,209,218)


visualizationForMonth : String -> Int -> Series Model Action
visualizationForMonth colorString monthIndex =
    let
        opacityValue =
            case monthIndex of
                0 ->
                    "1.0"

                1 ->
                    "0.45"

                _ ->
                    "0.2"
    in
        { axis = normalAxis
        , interpolation = Linear Nothing [ (stroke colorString), (opacity opacityValue) ]
        , toDataPoints = dataPointConverter colorString opacityValue monthIndex
        }


type alias Point =
    { x : Float
    , y : Float
    , t : Transaction
    }


dataPointConverter : String -> String -> Int -> Model -> List (DataPoint Action)
dataPointConverter colorString opacityValue monthIndex em =
    let
        stddevBoundary =
            Statistics.transactionStddevBoundary em.allTransactions

        boundary =
            boundaryForIndex em monthIndex

        inDateBoundary : Transaction -> Bool
        inDateBoundary t =
            Date.Extra.isBetween boundary.from boundary.to t.createdDate

        circleColor : String -> Transaction -> String
        circleColor defaultColorString t =
            if t == em.selectedTransaction then
                "rgb(16,163,163)"
                -- else if t.amount > stddevBoundary then
                --     "rgb(168,219,168)"
            else
                defaultColorString

        circleRadius : Transaction -> String
        circleRadius t =
            if t == em.selectedTransaction then
                "5"
            else
                "3"

        viewCircle : String -> Transaction -> Svg Action
        viewCircle defaultColorString t =
            Svg.circle
                [ t |> circleRadius |> r
                , stroke "transparent"
                , opacity opacityValue
                , t |> circleColor defaultColorString |> fill
                , onClick (Hover t)
                ]
                []

        mapWithCumulativeSum : Float -> List Point -> List Point
        mapWithCumulativeSum sum points =
            case points of
                head :: remainder ->
                    let
                        newSum =
                            sum + head.y
                    in
                        { head | y = newSum } :: (mapWithCumulativeSum newSum remainder)

                [] ->
                    []

        monthWidth =
            Date.Extra.diff Date.Extra.Day boundary.from boundary.to
                |> toFloat

        normalizeMonthLength x =
            100 * x / monthWidth

        toPoint : Transaction -> Point
        toPoint t =
            { x =
                Date.Extra.diff Date.Extra.Day boundary.from t.createdDate
                    |> toFloat

            -- |> normalizeMonthLength
            , y =
                case t.transactionType of
                    Earning ->
                        t.amount

                    Expense ->
                        -t.amount
            , t = t
            }
    in
        em.scopedTransactions
            |> List.filter inDateBoundary
            |> List.map toPoint
            |> mapWithCumulativeSum 0
            -- |> (::) { t = voidTransaction, x = 31, y = 0 }
            |> List.map (\p -> dot (viewCircle colorString p.t) p.x p.y)



--plotExample : PlotExample msg
--plotExample =
--    { title = "Sin"
--    , code = code
--    , view = view
--    , id = "PlotSine"
--    }
--
--
--data : List ( Float, Float )
--data =
--    List.map (\v -> ( toFloat v, sin (degrees <| toFloat v) )) (List.range 0 360)
--
--
--customLine : Series (List ( Float, Float )) msg
--customLine =
--    { axis = verticalAxis
--    , interpolation = Monotone Nothing [ stroke pinkStroke ]
--    , toDataPoints = List.map (\( x, y ) -> clear x y)
--    }
--
--
--verticalAxis : Axis
--verticalAxis =
--    customAxis <|
--        \summary ->
--            { position = Basics.min
--            , axisLine = Just (dataLine summary)
--            , ticks = List.map simpleTick (interval 0 0.5 summary)
--            , labels = List.map simpleLabel (interval 0 0.5 summary)
--            , flipAnchor = False
--            }
--
--


horizontalAxis : Model -> Axis
horizontalAxis em =
    let
        monthLabel : Date -> LabelCustomizations
        monthLabel d =
            { position = toTime d
            , view = d |> month |> toString |> viewLabel []
            }
    in
        customAxis <|
            \summary ->
                { position = (\_ _ -> 0)
                , axisLine = Just (dataLine summary)
                , ticks = [] --List.map simpleTick (List.map toTime boundaryStarts)
                , labels = [] --List.map monthLabel boundaryStarts
                , flipAnchor = False
                }


dataLine : AxisSummary -> LineCustomizations
dataLine summary =
    { attributes = [ stroke "grey" ]
    , start = summary.dataMin
    , end = summary.dataMax
    }


title : Svg msg
title =
    viewLabel
        [ fill axisColor
        , style
            [ ( "text-anchor", "end" )
            , ( "font-style", "italic;" )
            ]
        ]
        ""


axisColor : String
axisColor =
    "#afafaf"



--
--
--view : Svg.Svg a
--view =
--    viewSeriesCustom
--        { defaultSeriesPlotCustomizations
--            | horizontalAxis = horizontalAxis
--            , junk = \summary -> [ junk  summary.x.dataMax summary.y.max ]
--            , toDomainLowest = \y -> y - 0.25
--            , toRangeLowest = \y -> y - 25
--        }
--        [ customLine ]
--        data
