module Request exposing (..)

import Model exposing (Transaction, TransactionType)
import Http exposing (..)
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withHeader, withQueryParams)
import Json.Decode as Decode exposing (..)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline exposing (custom, decode, hardcoded, required, requiredAt)


getTransactions : Http.Request (List Transaction)
getTransactions =
    let
        _ =
            Debug.log "" "getTransactions was called"

        expect =
            Decode.list transactionDecoder
                |> Decode.at [ "response", "transactions" ]
                |> Http.expectJson
    in
        "https://api.nordeaopenbanking.com/v1/accounts/<TODO>/transactions"
            |> HttpBuilder.get
            |> withAuthorization
            |> HttpBuilder.withExpect expect
            |> HttpBuilder.toRequest


withAuthorization : RequestBuilder a -> RequestBuilder a
withAuthorization builder =
    builder
        |> withHeader "X-IBM-Client-Id" "<TODO>"
        |> withHeader "X-IBM-Client-Secret" "<TODO>"
        |> withHeader "Authorization" "Bearer authenticated-user-full-access"


transactionDecoder : Decode.Decoder Transaction
transactionDecoder =
    decode Transaction
        |> requiredAt [ "bookingDate", "date" ] Json.Decode.Extra.date
        |> requiredAt [ "valueDate", "date" ] Json.Decode.Extra.date
        |> requiredAt [ "creditDebitIndicator" ] transactionTypeDecoder
        |> requiredAt [ "amount", "value" ] amountDecoder
        |> requiredAt [ "entryDetails", "transactionDetails", "relatedParties" ] (Decode.oneOf [ creditorDecoder, debtorDecoder ])
        |> hardcoded 0


transactionTypeDecoder : Decode.Decoder TransactionType
transactionTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "CRDT" ->
                        Decode.succeed Model.Earning

                    "DBIT" ->
                        Decode.succeed Model.Expense

                    somethingElse ->
                        Decode.fail <| "Unknown creditDebitIndicator: " ++ somethingElse
            )


amountDecoder : Decode.Decoder Float
amountDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case (String.toFloat str) of
                    Ok f ->
                        Decode.succeed f

                    Err message ->
                        Decode.fail message
            )


creditorDecoder : Decode.Decoder (List String)
creditorDecoder =
    Decode.at [ "creditor" ] nameDecoder


debtorDecoder : Decode.Decoder (List String)
debtorDecoder =
    Decode.at [ "debtor" ] nameDecoder


nameDecoder : Decode.Decoder (List String)
nameDecoder =
    Decode.at [ "name" ] Decode.string
        |> Decode.andThen
            (\str ->
                Decode.succeed [ str ]
            )
