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


viewHelperText : Element Msg
viewHelperText =
    column [ width <| maximum 800 fill, centerX, spacing small ]
        [ paragraph []
            [ text """At the moment it is possible to jump into the pool directly before an airdrop snapshot.
                   This discriminates poolers which are providing liquidity since a long time.
                   To counteract last-minute pooling it is probably a good idea to reward long-term pooling.

                   """
            ]
        , paragraph []
            [ text """We could implement this by using daily snapshots of the LP tokens of each holder
                              (in this demo we use weekly snapshots for easier exploration).
                              To give long-term holdings even more weight, we can use a weighted average of these snapshots.
                              The weighting function is visualized below (blue line)."""
            ]
        , paragraph []
            [ text "To understand the concept it is only necessary to know how the weighting function looks like, but if you are interested in the math: timeWeight(x) = e ^ (-1 * (stretch * (x - shift)) ^ 2)"
            ]
        , paragraph []
            [ text """You can change the function parameters "Time Weight Shift Factor" (shift)
                                and "Time Weight Stretch Factor" (stretch), but be careful and only change in small steps.
                                By the way, using a constant timeWeight function would be identical to Average Daily Share Holdings (ADSH),
                                therefore you can think of this concept as ADSH on steroids with dynamic time weights."""
            ]
        , paragraph []
            [ text """To calculate the actual airdrop share, we need to calculate the so called aidropCredit first.
                                It is the weighted sum of all valid LP token balances with balances_i identifying the i-th balance
                                (balances_0 is the youngest and balances_52 the oldest balance):"""
            ]
        , paragraph []
            [ text """airdropCredit(balances) = Sum{i=1, i ∈ [1, 52]} (timeWeight(i) * balances_i)"""
            ]
        , paragraph []
            [ text """(e.g. airdropCredit([4,3,2,1]) = 0.5 * 4 + 0.55 * 3 + 0.6 * 2 + 0.65 * 1) """
            ]
        , paragraph []
            [ text """
                    Until now we only considered DGVC LP pooling, but for the DegenVC project to succeed we also need pooling for the airdropped tokens.
                    To encourage pooling for airdropped tokens, we could reward the poolers (who are taking the risk) in the following airdrops.

                    To implement this we would calculate an independent airdropCredit for each LP pool (DGVC and e.g. up to 4 airdropped tokens).
                    Then we could calculate the weighted sum as weightedAirdropCredit with e.g. the following weights: 60% DGVC and 10% each of the airdopped token pools.

                    Finally, we can calculate the proportional share of the airdrop (nothing special here):
                    """
            ]
        , paragraph []
            [ text "totalAirdropCredits = sum of weightedAirdropCredits of all liquidity providers"
            ]
        , paragraph []
            [ text "airdropShare = airdropCredit / totalAirdropCredits"
            ]
        , paragraph []
            [ text """(airdropShare is not calculated in this demo because we would need to know the weightedAirdropCredit of all other holders)"""
            ]
        , paragraph []
            [ text """Enough of the theory behind. Feel free to play around with the airdrop calculator below.
                            Just start by providing weekly token balances (type in a list of arbitrary balances you want to explore).
                            The amount of balances is not limited directly, but only the last 52 weekly balances will be considered for the calculation.
                            Removing liquidity from the pool will reduce all previous balances to the same amount (removing all from pooling would reduce all previous balances to zero).
                            You can see the valid token balances below the input field.
                            """
            ]
        , paragraph []
            [ text """The three main questions for the community to figure out are:"""
            ]
        , paragraph []
            [ text """(1) How much weight should the youngest balance have (e.g. 0.5)?
                            This shouldn't be too low because otherwise new poolers will maybe shy away from joining.
                            """
            ]
        , paragraph []
            [ text """(2) When should be the maximum weight (e.g. at 12 weeks)?
                            This should be early enough to incentive long-term pooling,
                            but on the other hand shouldn't be too far in the past that new poolers can reach the maximum weight in a reasonable amount of time.
                            """
            ]
        , paragraph []
            [ text """(3) When should the weight decrease to zero (e.g. around a year)?
                                At some time the weight needs to decrease again otherwise it would be unfair to new poolers.
                                Keep in mind that a year in crypto space corresponds to a decade in more traditional areas.
                            """
            ]
        , paragraph []
            [ text """If you have any questions feel free to reach out to me.
                            I am really looking forward to your feedback and the discussion with you afterwards.
                            I hope that the project and the #degenhorde benefits from this long-term pooling reward concept."""
            ]
        , paragraph []
            [ text """
                            Hint 1: the token balances are chronological and the graphs are antichronological
                            """
            ]
        , paragraph []
            [ text """
                            Hint 2: the green line in the second graph is the weighted balance.
                            Each weekly balance (red line) is multiplied by the corresponding weight (blue line).
                            The area under the curve of the weighted balance (green) is the airdropCredit function.
                            """
            ]
        , paragraph []
            [ text """
                            Hint 3: this tool is only for exploration and educational purposes to support the process of
                            designing a proper long-term pooling reward.
                            After the community and of course the team committed to one concrete approach we can implement
                            an automatic tracking tool connected to the Ethereum blockchain. So everybody can check his/her airdrop share at anytime.
                            I also already started to integrate the real (historical) LP token balances for automatic tracking by connecting to Etherscan.
                            """
            ]
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
            [ viewHelperText
            , viewDailyLPBalances model.dgvcLPBalances model.dgvcLPBalancesRaw
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
