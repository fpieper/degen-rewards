module Main exposing (..)

import Browser
import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Palette exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- HELPERS


roundDecimals : Int -> Float -> Float
roundDecimals decimals number =
    (number * toFloat (10 ^ decimals) |> round |> toFloat) / toFloat (10 ^ decimals)



-- MODEL


type alias Model =
    { dgvcLPBalances : List Float
    , dgvcLPBalancesRaw : String
    , airdrop1LPBalances : List Float
    , airdrop1LPBalancesRaw : String
    , airdrop2LPBalances : List Float
    , airdrop2LPBalancesRaw : String
    , timeCreditStretch : Float
    , timeCreditStretchRaw : String
    , timeCreditShift : Int
    , timeCreditShiftRaw : String
    }


init : Model
init =
    { dgvcLPBalances = []
    , dgvcLPBalancesRaw = ""
    , airdrop1LPBalances = []
    , airdrop1LPBalancesRaw = ""
    , airdrop2LPBalances = []
    , airdrop2LPBalancesRaw = ""
    , timeCreditStretch = 0.07
    , timeCreditStretchRaw = "0.07"
    , timeCreditShift = 12
    , timeCreditShiftRaw = "12"
    }



-- 12 and 0.08 for weekly
-- 90 and 0.01 for daily


parseBalances : String -> List Float
parseBalances balances =
    balances
        |> String.split ","
        |> List.map String.trim
        |> List.filterMap String.toFloat
        |> List.filter (\balance -> balance >= 0)


limitBalances : List Float -> List Float
limitBalances balances =
    let
        appendNextOrMin : Float -> ( List Float, Float ) -> ( List Float, Float )
        appendNextOrMin next ( elements, minimum ) =
            let
                newMinimum =
                    min next minimum
            in
            ( newMinimum :: elements, newMinimum )
    in
    case List.reverse balances of
        x :: xs ->
            List.foldl appendNextOrMin ( [ x ], x ) xs
                |> Tuple.first

        _ ->
            balances


timeCredit : Int -> Float -> Int -> Float
timeCredit shift stretch day =
    e ^ (-1 * (stretch * toFloat (day - shift)) ^ 2)


airdropCredit : Int -> Float -> List Float -> ( Float, Float )
airdropCredit shift stretch balances =
    let
        credit : Float
        credit =
            balances
                |> List.reverse
                |> List.indexedMap (\i balance -> balance * timeCredit shift stretch i)
                |> List.sum

        creditLastDay : Float
        creditLastDay =
            balances
                |> List.reverse
                |> List.head
                |> Maybe.withDefault 0
                |> (*) (timeCredit shift stretch 0)

        boost : Float
        boost =
            if creditLastDay == 0 then
                0

            else
                ((credit / creditLastDay) - 1) * 100
    in
    ( roundDecimals 2 credit, roundDecimals 2 boost )



-- UPDATE


type Msg
    = DailyLPBalancesChanged String
    | TimeCreditStretchChanged String
    | TimeCreditShiftChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        DailyLPBalancesChanged balances ->
            { model | dgvcLPBalances = limitBalances <| parseBalances balances, dgvcLPBalancesRaw = balances }

        TimeCreditStretchChanged stretch ->
            case String.toFloat stretch of
                Just parsedStretch ->
                    { model | timeCreditStretch = parsedStretch, timeCreditStretchRaw = stretch }

                Nothing ->
                    { model | timeCreditStretchRaw = stretch }

        TimeCreditShiftChanged shift ->
            case String.toInt shift of
                Just parsedShift ->
                    { model | timeCreditShift = parsedShift, timeCreditShiftRaw = shift }

                Nothing ->
                    { model | timeCreditShiftRaw = shift }



-- VIEW


inputStyles : List (Attribute Msg)
inputStyles =
    [ padding small
    , focused [ Border.color freeSpeechBlue ]
    , Border.rounded 3
    , Border.width 2
    , Border.color <| blackAlpha 0.3
    ]


viewDailyLPBalances : List Float -> String -> Element Msg
viewDailyLPBalances balances rawBalances =
    let
        renderedBalances =
            balances
                |> List.map String.fromFloat
                |> String.join ", "
    in
    column [ spacing small, width fill ]
        [ Input.text inputStyles
            { onChange = DailyLPBalancesChanged
            , text = rawBalances
            , placeholder = Just <| Input.placeholder [] <| text "1.2, 2, 5.4, 19, 0"
            , label =
                Input.labelAbove [] <|
                    row [ spacing xSmall ]
                        [ text "DGVC LP token balances"
                        , el [ Font.color <| blackAlpha 0.5 ] <| text "(chronologically, weekly - last element corresponds to the day before the airdrop)"
                        ]
            }
        , if not (String.isEmpty renderedBalances) then
            column [ spacing xSmall ]
                [ text "Valid DGVC LP token balances: "
                , el [ paddingXY small 0 ] <| text renderedBalances
                ]

          else
            none
        ]


viewTimeCreditStretch : Float -> String -> Element Msg
viewTimeCreditStretch stretch rawStretch =
    Input.text inputStyles
        { onChange = TimeCreditStretchChanged
        , text = rawStretch
        , placeholder = Nothing
        , label =
            Input.labelAbove [ spacing xSmall ] <| text "Time Weight Stretch Factor"
        }


viewTimeCreditShift : Int -> String -> Element Msg
viewTimeCreditShift shift rawShift =
    Input.text inputStyles
        { onChange = TimeCreditShiftChanged
        , text = rawShift
        , placeholder = Nothing
        , label =
            Input.labelAbove [ spacing xSmall ] <| text "Time Weight Shift Factor"
        }


viewResults : Model -> Element Msg
viewResults model =
    let
        ( dgvcAirdropCredit, dgvcBoost ) =
            airdropCredit model.timeCreditShift model.timeCreditStretch model.dgvcLPBalances
    in
    Element.table [ spacing normal, paddingXY 0 normal ]
        { data =
            [ { label = "Airdrop Credits:"
              , value =
                    row [ spacing xSmall ]
                        [ text <| String.fromFloat dgvcAirdropCredit
                        , el [ Font.color <| blackAlpha 0.5 ] <| text "(area under green curve)"
                        ]
              }
            , { label = "Holding Boost:"
              , value =
                    row [ spacing xSmall ]
                        [ text <| String.fromFloat dgvcBoost ++ " %"
                        , el [ Font.color <| blackAlpha 0.5 ] <| text "(compared to pooling in last week only)"
                        ]
              }
            ]
        , columns =
            [ { header = none
              , width = shrink
              , view =
                    \result ->
                        text result.label
              }
            , { header = none
              , width = fill
              , view =
                    \result ->
                        result.value
              }
            ]
        }


viewTimeCreditGraph : Int -> Float -> Element Msg
viewTimeCreditGraph shift stretch =
    let
        timeCredits =
            List.range 0 52
                |> List.map (\i -> ( toFloat i, timeCredit shift stretch i ))
    in
    el [ width fill ] <|
        html <|
            LineChart.viewCustom
                { x = Axis.default 700 "Week" Tuple.first
                , y = Axis.default 400 "" Tuple.second
                , container = Container.default "line-chart-1"
                , interpolation = Interpolation.default
                , intersection = Intersection.default
                , legends = Legends.default
                , events = Events.default
                , junk = Junk.default
                , grid = Grid.default
                , area = Area.default
                , line = Line.default
                , dots = Dots.default
                }
                [ LineChart.line Color.blue Dots.none "Weight" timeCredits
                ]


viewTimeCreditBalances : Int -> Float -> List Float -> Element Msg
viewTimeCreditBalances shift stretch balances =
    let
        paddedReverseBalance =
            List.reverse <| 0 :: List.take 52 balances

        timeCredits : List Float
        timeCredits =
            List.range 0 52
                |> List.map (\i -> timeCredit shift stretch i)

        weightedBalances : List ( Float, Float )
        weightedBalances =
            List.map2 Tuple.pair timeCredits paddedReverseBalance
                |> List.indexedMap (\index ( weight, balance ) -> ( toFloat index, weight * balance ))

        maximumBalance =
            Maybe.withDefault 0 <| List.maximum balances

        maximumWeightedBalance =
            Maybe.withDefault 0 <| (List.maximum <| List.map (\( index, weighted ) -> weighted) weightedBalances)

        yAxis =
            Axis.custom
                { title = Title.default ""
                , variable = Just << Tuple.second
                , pixels = 400
                , range =
                    Range.window 0 (max 1 <| max maximumBalance maximumWeightedBalance)
                , axisLine =
                    AxisLine.rangeFrame Color.gray
                , ticks = Ticks.int 7
                }
    in
    el [ width fill ] <|
        html <|
            LineChart.viewCustom
                { x = Axis.default 700 "Week" Tuple.first
                , y = yAxis
                , container = Container.default "line-chart-1"
                , interpolation = Interpolation.default
                , intersection = Intersection.default
                , legends = Legends.default
                , events = Events.default
                , junk = Junk.default
                , grid = Grid.default
                , area = Area.default
                , line = Line.default
                , dots = Dots.default
                }
                [ LineChart.line Color.red
                    Dots.none
                    "Balance"
                    (List.indexedMap (\index balance -> ( toFloat index, balance )) <| paddedReverseBalance)
                , LineChart.line Color.green Dots.none "Weighted" weightedBalances
                ]


view : Model -> Html.Html Msg
view model =
    layout
        [ paddingXY xLarge large
        , Font.size small
        , Font.color <| blackAlpha 0.8
        ]
    <|
        column [ spacing normal, width fill ]
            [ viewDailyLPBalances model.dgvcLPBalances model.dgvcLPBalancesRaw
            , row [ width fill, spacing normal ]
                [ viewTimeCreditShift model.timeCreditShift model.timeCreditShiftRaw
                , viewTimeCreditStretch model.timeCreditStretch model.timeCreditStretchRaw
                ]
            , wrappedRow [ width fill ]
                [ viewTimeCreditGraph model.timeCreditShift model.timeCreditStretch
                , viewTimeCreditBalances model.timeCreditShift model.timeCreditStretch model.dgvcLPBalances
                ]
            , viewResults model
            ]
