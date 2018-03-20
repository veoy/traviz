module Stylesheet exposing (..)

import Color
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color exposing (border)
import Style.Font as Font
import Style.Transition as Transition


-- TODO: Revisit


type Styles
    = Box
      -- Mine
    | None
    | Plot
    | Control
    | DeactivatedButton -- TODO
    | Button
    | Combo
    | ActiveComboChoice
    | InactiveComboChoice
    | TransactionRow
    | TransactionInfoRow
    | PositiveTransactionRow
    | NegativeTransactionRow
    | SelectedTransactionRow
    | TransactionTable
    | TransactionTableHeading
    | Card
    | Header



-- TODO - Problems with style elements
-- - Styles are not "type-safe". If one is removed, there is simply no style for that element


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ Style.style None [] -- It's handy to have a blank style
        , Style.style Plot
            [ Color.background Color.white
            ]
        , Style.style Control
            [ Color.border Color.grey

            -- , Border.all 1
            ]
        , Style.style Header
            [ Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , Font.size 14
            , Font.bold
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            ]
        , Style.style Card
            [ Color.border Color.grey
            , Border.all 1
            , Border.rounded 10
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , Font.size 14
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            ]
        , Style.style Button
            [ Color.background (Color.rgb 33 133 208)
            , Transition.all
            , Color.text Color.white
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , Font.size 16
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            , Border.rounded 10
            , hover
                [ Color.background (Color.rgb 22 120 194)
                , cursor "pointer"
                ]
            , pseudo "active"
                [ Color.background (Color.rgb 26 105 164) ]
            ]
        , Style.style Combo
            [ Color.background (Color.rgb 26 105 164)
            , Border.rounded 10
            ]
        , Style.style ActiveComboChoice
            [ Color.background (Color.rgb 33 133 208)
            , Transition.all
            , Color.text Color.white
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , Font.size 16
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            , Border.rounded 10
            ]
        , Style.style InactiveComboChoice
            [ Color.text Color.white
            , Transition.all
            , Font.typeface [ "helvetica", "arial", "sans-serif" ]
            , Font.size 16
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            , Border.rounded 10
            , hover
                [ Color.background (Color.rgb 22 120 194)
                , cursor "pointer"
                ]
            , pseudo "active"
                [ Color.background (Color.rgb 33 133 208) ]
            ]
        , Style.style TransactionRow
            [ Border.right 1
            , Border.bottom 1
            , Transition.all
            , Color.border (Color.rgba 34 36 38 0.15)
            , Style.cursor "pointer"
            ]
        , Style.style TransactionInfoRow
            [ Border.right 1
            , Border.bottom 1
            , Transition.all
            , Color.border (Color.rgba 34 36 38 0.15)
            , Color.background (Color.rgba 0 0 50 0.02)
            , Style.cursor "pointer"
            ]
        , Style.style PositiveTransactionRow
            [ Border.right 1
            , Border.bottom 1
            , Transition.all
            , Color.text (Color.rgb 44 102 45)
            , Color.background (Color.rgb 252 255 245)
            , Color.border (Color.rgba 34 36 38 0.15)
            , Style.cursor "pointer"
            ]
        , Style.style NegativeTransactionRow
            [ Border.right 1
            , Border.bottom 1
            , Transition.all
            , Color.border (Color.rgba 34 36 38 0.15)
            , Color.text (Color.rgb 159 58 56)
            , Color.background (Color.rgb 255 246 246)
            , Style.cursor "pointer"
            ]
        , Style.style SelectedTransactionRow
            [ Border.right 1
            , Border.bottom 1
            , Transition.all
            , Color.border (Color.rgba 34 36 38 0.15)
            , Color.text (Color.rgb 16 163 163)
            , Color.background (Color.rgb 225 247 247)
            , Style.cursor "pointer"
            ]
        , Style.style TransactionTable
            [ Border.top 1
            , Border.left 1
            , Transition.all
            , Color.border (Color.rgba 34 36 38 0.15)
            ]
        , Style.style TransactionTableHeading
            [ Border.right 1
            , Border.bottom 1
            , Transition.all
            , Color.background (Color.rgb 249 250 251)
            , Color.border (Color.rgba 34 36 38 0.15)
            ]
        ]
