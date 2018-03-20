module Main exposing (..)

import Controller exposing (..)
import Model exposing (..)
import Html exposing (beginnerProgram)
import View exposing (view)


--Stian: "Folk er ikke interessert i å få oversikt. De vil kun vite hvor mye penger de har igjen"
-- Potential TODOs:
--  Simplified view with only net-change per month
--  Temperatur måler: Hvordan er økonomien site måned sammenliknet med foregående.
--  Emoji reaksjon måned for måned
--  Mulighet for å laste opp transaksjoner fra ulike providers
--  Get inspired by highcharts
--  Should we equalize width of the graphs, or use number of days as now
--  Use tuple in a case-expression for something :D
--  Make the graph work on days, but show a small listing of the transactions onHover
--    This means we don't merge the transactions. The graph view merges them.
--    What is then the "currentTransaction"-expression? Current date? null matches all?
--  Salary period ends with salary to normalize beginning?
--  Eivind: Legend på linjer, navn på akser, google Dashboard design, skravere område under grafen, myke linjer istedenfor harde, prosent-guidelines
--  Is Runway an intuitive concept? (Number of Days/months/years you can survive on your current balance given no income)


main : Program Never Model.Model Action
main =
    Html.program
        { init = initialModel ! []

        -- , view = View
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
