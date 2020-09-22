module Main exposing (..)

import BigInt
import Browser
import Element exposing (..)
import Html
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import OrderedDict exposing (OrderedDict)
import RemoteData exposing (RemoteData)
import Time exposing (Posix)
import Url.Builder exposing (absolute, crossOrigin)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { apiKey : String
    , pools : OrderedDict String Pool
    }


type alias Pool =
    { name : String
    , transactions : RemoteData Http.Error (List Transaction)
    }


type alias Transaction =
    { timeStamp : String --Posix
    , from : String
    , to : String
    , decimals : String --Int
    , value : String --BigInt.BigInt
    }


init : () -> ( Model, Cmd Msg )
init flags =
    let
        apiKey =
            "1TP4S4C6I5SD3SDH9G5S1QUN7IVXXY7WBX"

        pools =
            OrderedDict.fromList
                [ ( "0x7cdc560cc66126a5eb721e444abc30eb85408f7a"
                  , { name = "DGVC"
                    , transactions = RemoteData.NotAsked
                    }
                  )
                ]
    in
    ( { apiKey = apiKey
      , pools = pools
      }
    , loadTransactions apiKey pools
    )


loadTransactions : String -> OrderedDict String Pool -> Cmd Msg
loadTransactions apiKey pools =
    pools
        |> OrderedDict.keys
        |> List.map
            (\address ->
                let
                    etherscan =
                        crossOrigin "https://api.etherscan.io"
                            [ "api" ]
                            [ Url.Builder.string "module" "account"
                            , Url.Builder.string "action" "tokentx"
                            , Url.Builder.string "contractaddress" address
                            , Url.Builder.string "sort" "asc"
                            , Url.Builder.string "apiKey" apiKey
                            , Url.Builder.int "page" 1
                            , Url.Builder.int "offset" 10000
                            ]

                    localTest =
                        absolute [ "etherscan", "api.json" ] []
                in
                Http.get
                    { url = localTest
                    , expect = Http.expectJson (GotTransactions address) transactionsDecoder
                    }
            )
        |> Cmd.batch


transactionDecoder : Decoder Transaction
transactionDecoder =
    Decode.map5 Transaction
        (field "timeStamp" string)
        (field "from" string)
        (field "to" string)
        (field "tokenDecimal" string)
        (field "value" string)


transactionsDecoder : Decoder (List Transaction)
transactionsDecoder =
    field "result"
        (list transactionDecoder)



-- UPDATE


type Msg
    = GotTransactions String (Result Http.Error (List Transaction))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTransactions address result ->
            let
                updateTransactions transactions =
                    { model
                        | pools =
                            OrderedDict.update address
                                (Maybe.map (\pool -> { pool | transactions = transactions }))
                                model.pools
                    }
            in
            case result of
                Ok transactions ->
                    ( updateTransactions <| RemoteData.Success transactions
                    , Cmd.none
                    )

                Err error ->
                    ( updateTransactions <| RemoteData.Failure error
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
    layout [] <|
        paragraph []
            [ text <|
                Debug.toString model.pools
            ]
