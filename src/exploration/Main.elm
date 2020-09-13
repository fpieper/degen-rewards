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
    , dgvcShare : Float
    , dgvcShareRaw : String
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
    , dgvcShare = 0.6
    , dgvcShareRaw = "0.6"
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


weightedAirdropCredit : Float -> Float -> Float -> Float -> Float
weightedAirdropCredit dgvcShare dgvc airdrop1 airdrop2 =
    if dgvc == 0 then
        0

    else
        let
            airdropShare =
                (1 - dgvcShare) / 2
        in
        roundDecimals 2 <| dgvc * dgvcShare + airdrop1 * airdropShare + airdrop2 * airdropShare



-- UPDATE


type Msg
    = DailyLPBalancesChanged String
    | Airdrop1LPBalancesChanged String
    | Airdrop2LPBalancesChanged String
    | TimeCreditStretchChanged String
    | TimeCreditShiftChanged String
    | DgvcShareChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        DailyLPBalancesChanged balances ->
            { model | dgvcLPBalances = limitBalances <| parseBalances balances, dgvcLPBalancesRaw = balances }

        Airdrop1LPBalancesChanged balances ->
            { model | airdrop1LPBalances = limitBalances <| parseBalances balances, airdrop1LPBalancesRaw = balances }

        Airdrop2LPBalancesChanged balances ->
            { model | airdrop2LPBalances = limitBalances <| parseBalances balances, airdrop2LPBalancesRaw = balances }

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

        DgvcShareChanged share ->
            case String.toFloat share of
                Just parsedShare ->
                    { model | dgvcShare = parsedShare, dgvcShareRaw = share }

                Nothing ->
                    { model | dgvcShareRaw = share }



-- VIEW


inputStyles : List (Attribute Msg)
inputStyles =
    [ padding small
    , focused [ Border.color freeSpeechBlue ]
    , Border.rounded 3
    , Border.width 2
    , Border.color <| blackAlpha 0.3
    ]


viewDailyLPBalances : (String -> Msg) -> String -> List Float -> String -> Element Msg
viewDailyLPBalances msg label balances rawBalances =
    let
        renderedBalances =
            balances
                |> List.map String.fromFloat
                |> String.join ", "
    in
    column [ spacing small, width fill ]
        [ Input.text inputStyles
            { onChange = msg
            , text = rawBalances
            , placeholder = Just <| Input.placeholder [] <| text "1.2, 2, 5.4, 19, 0"
            , label =
                Input.labelAbove [] <|
                    row [ spacing xSmall ]
                        [ text <| label ++ " LP token balances"
                        , el [ Font.color <| blackAlpha 0.5 ] <| text "(chronologically, weekly - last element corresponds to the day before the airdrop)"
                        ]
            }
        , if not (String.isEmpty renderedBalances) then
            column [ spacing xSmall ]
                [ text <| "Valid " ++ label ++ " LP token balances: "
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


viewDgvcShare : Float -> String -> Element Msg
viewDgvcShare share shareRaw =
    Input.text inputStyles
        { onChange = DgvcShareChanged
        , text = shareRaw
        , placeholder = Nothing
        , label =
            Input.labelAbove [] <|
                row [ spacing xSmall ]
                    [ text <| "DGVC share"
                    , el [ Font.color <| blackAlpha 0.5 ] <| text "(used for weighting airdrop shares of different LP pools)"
                    ]
        }


viewResults : Model -> Element Msg
viewResults model =
    let
        ( dgvcAirdropCredit, dgvcBoost ) =
            airdropCredit model.timeCreditShift model.timeCreditStretch model.dgvcLPBalances

        ( airdrop1AirdropCredit, airdrop1Boost ) =
            airdropCredit model.timeCreditShift model.timeCreditStretch model.airdrop1LPBalances

        ( airdrop2AirdropCredit, airdrop2Boost ) =
            airdropCredit model.timeCreditShift model.timeCreditStretch model.airdrop2LPBalances
    in
    Element.table [ spacing normal, paddingXY 0 normal ]
        { data =
            [ { label = "Airdrop Credits (DGVC):"
              , value =
                    row [ spacing xSmall ]
                        [ text <| String.fromFloat dgvcAirdropCredit
                        , el [ Font.color <| toUiColor Color.red ] <| text "(area under red curve)"
                        ]
              }
            , { label = "Airdrop Credits (Airdrop 1):"
              , value =
                    row [ spacing xSmall ]
                        [ text <| String.fromFloat airdrop1AirdropCredit
                        , el [ Font.color <| toUiColor Color.yellow ] <| text "(area under yellow curve)"
                        ]
              }
            , { label = "Airdrop Credits (Airdrop 2):"
              , value =
                    row [ spacing xSmall ]
                        [ text <| String.fromFloat airdrop2AirdropCredit
                        , el [ Font.color <| toUiColor Color.green ] <| text "(area under green curve)"
                        ]
              }

            --, { label = "Holding Boost (DGVC):"
            --  , value =
            --        row [ spacing xSmall ]
            --            [ text <| String.fromFloat dgvcBoost ++ " %"
            --            , el [ Font.color <| blackAlpha 0.5 ] <| text "(compared to pooling in last week only)"
            --            ]
            --  }
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


viewTimeCreditBalances : Int -> Float -> List Float -> List Float -> List Float -> Element Msg
viewTimeCreditBalances shift stretch dgvcBalances airdrop1Balances airdrop2Balances =
    let
        timeCredits : List Float
        timeCredits =
            List.range 0 52
                |> List.map (\i -> timeCredit shift stretch i)

        padReverseBalances : List Float -> List Float
        padReverseBalances balances =
            List.reverse <| 0 :: List.take 52 balances

        weightBalances : List Float -> List ( Float, Float )
        weightBalances balances =
            List.map2 Tuple.pair timeCredits balances
                |> List.indexedMap (\index ( weight, balance ) -> ( toFloat index, weight * balance ))

        indexBalances : List Float -> List ( Float, Float )
        indexBalances balances =
            List.indexedMap (\index balance -> ( toFloat index, balance )) <| balances

        maximumBalance =
            Maybe.withDefault 0 <| List.maximum dgvcBalances

        --maximumWeightedBalance =
        --    Maybe.withDefault 0 <| (List.maximum <| List.map (\( index, weighted ) -> weighted) weightedBalances)
        yAxis =
            Axis.custom
                { title = Title.default ""
                , variable = Just << Tuple.second
                , pixels = 400
                , range =
                    Range.default

                --Range.window 0 (max 1 <| max maximumBalance maximumWeightedBalance)
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
                , legends = Legends.grouped .max .min -50 -280
                , events = Events.default
                , junk = Junk.default
                , grid = Grid.default
                , area = Area.default
                , line = Line.default
                , dots = Dots.default
                }
                [ LineChart.line Color.darkRed Dots.none "DGVC (Balance)" (indexBalances <| padReverseBalances <| dgvcBalances)
                , LineChart.line Color.red Dots.none "DGVC (Weighted)" (weightBalances <| padReverseBalances <| dgvcBalances)
                , LineChart.line Color.darkYellow Dots.none "Air1 (Balance)" (indexBalances <| padReverseBalances <| airdrop1Balances)
                , LineChart.line Color.yellow Dots.none "Air1 (Weighted)" (weightBalances <| padReverseBalances <| airdrop1Balances)
                , LineChart.line Color.darkGreen Dots.none "Air2 (Balance)" (indexBalances <| padReverseBalances <| airdrop2Balances)
                , LineChart.line Color.green Dots.none "Air2 (Weighted)" (weightBalances <| padReverseBalances <| airdrop2Balances)
                ]


viewHelperText : Element Msg
viewHelperText =
    column [ width <| maximum 900 fill, centerX, spacing small ]
        [ el [ Font.size normal ] <| text "DegenVC Long-Term Pooling Reward Calculator"
        , paragraph [] []
        , paragraph []
            [ text """At the moment it is possible to jump into the pool directly before an airdrop snapshot.
                   This discriminates poolers who have been providing liquidity for a long time.
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
            [ text """To calculate the actual airdrop share, we need to calculate the so called airdrop credits first.
                      It is the weighted sum of all valid LP token balances with balances_i identifying the i-th balance
                      (balances_0 is the youngest and balances_52 the oldest balance):"""
            ]
        , paragraph []
            [ text """airdropCredits(balances) = Sum{i=1, i ∈ [1, 52]} (timeWeight(i) * balances_i)"""
            ]
        , paragraph []
            [ text """(e.g. airdropCredits([4,3,2,1]) = 0.5 * 4 + 0.55 * 3 + 0.6 * 2 + 0.65 * 1) """
            ]
        , paragraph []
            [ text """
                    Until now we only considered DGVC pooling, but for the DegenVC project to succeed, we also need pooling for the airdropped tokens.
                    To encourage pooling for airdropped tokens, we could reward the poolers (who are taking the risk) in the following airdrops.

                    """ ]
        , paragraph []
            [ text """To implement this we would independently calculate the airdrop credits for each LP pool
                    (DGVC and airdropped tokens).
                    Then we split the total amount of new airdrops into multiple funds with corresponding fund weights:
                    e.g. 50% for pooling DGVC and the remaining 50% is evenly distributed for pooling the airdropped token pools.
                    This is also safe compared to using a weighted sum of the airdrop credits and using
                    that for calculating an overall airdrop share because in a weighted sum it would be possible
                    for someone who pools a huge sum (e.g. a billion $) in one airdropped pool to get nearly all new airdrops.
                    """
            ]
        , paragraph []
            [ text """Also having an even fund share between the previous airdrop pools encourages the poolers to evenly support each project.
                    The idea is simple: you gain most airdrops by putting them in the smallest pool.
                    In the extreme case if you would be the only pooler in one pool, you will get all new airdrops assigned to that pool
                    (e.g. 12.5% of all new airdrops assuming four past airdrop pools).
                    In the end, having higher gains in smaller pools and smaller gains in larger pools leads to an equilibrium.
                    """
            ]
        , paragraph []
            [ text """Still, you always need to be part of the DGVC pool for qualifying to get rewards for the airdropped pools.
                    Finally, we can calculate the proportional airdrop share of each fund and the final weighted airdrop share for the payout (nothing special here):"""
            ]
        , paragraph []
            [ text "totalAirdropCredits = sum of airdropCredits of all liquidity providers (per fund or pool)"
            ]
        , paragraph []
            [ text "airdropShare = airdropCredits / totalAirdropCredits (per fund or pool)"
            ]
        , paragraph []
            [ text "weightedAirdropShare(airdropShares, fundWeights) = Sum{i=1, i ∈ [1, number of airdrop funds]} (airdropShares_i * fundWeights_i)"
            ]
        , paragraph []
            [ text """(airdrop shares are not calculated in this demo because we would need to know the total airdrop credits per fund of all other holders)"""
            ]
        , paragraph [ padding small ]
            []
        , paragraph []
            [ text """Enough of theory. Feel free to play around with the airdrop calculator below.
                            Just start by providing weekly token balances (type in a list of arbitrary balances you want to explore).
                            The amount of balances is not limited directly, but only the last 52 weekly balances will be considered for the calculation.
                            Removing liquidity from the pool will reduce all previous balances to the same amount (removing all from pooling would reduce all previous balances to zero).
                            You can see the valid token balances below the input field.
                            """
            ]
        , paragraph []
            [ text """The five main questions for the community to figure out are:"""
            ]
        , paragraph []
            [ text """(1) How much weight should the youngest balance have (e.g. 0.5)?
                            This shouldn't be too low because otherwise new poolers will maybe shy away from joining.
                            """
            ]
        , paragraph []
            [ text """(2) When should be the maximum weight (e.g. at 12 weeks)?
                            This should be early enough to incentive long-term pooling,
                            but on the other hand shouldn't be too far in the past that
                             new poolers can reach the maximum weight in a reasonable amount of time.
                            """
            ]
        , paragraph []
            [ text """(3) When should the weight decrease to zero (e.g. around a year)?
                                At some time the weight needs to decrease again otherwise it would be unfair to new poolers.
                                Keep in mind that a year in crypto space corresponds to a decade in more traditional areas.
                            """
            ]
        , paragraph []
            [ text """(4) How many previous airdrops should we consider (e.g. 5-10)?
                          This should correspond to a proper seed support phase,
                          meaning we can stop rewarding pooling for a project if it can stand on its own feet.
                   """
            ]
        , paragraph []
            [ text """(5) What should be the minimum DGVC share (e.g. 50%) and the maximum share of each airdropped pool (e.g. 20%)?
                          The idea is that DGVC as main pool should be more important, also the influence of one airdropped pool should be limited.
                          However, the maximum share of an airdropped pool is only relevant in the beginning
                          until an amount of three previous airdrop pools (assuming a maximum airdrop share of 20% and a minimum DGVC share of 50%).
                   """
            ]
        , paragraph []
            [ text """

                        These are my thoughts regarding rewarding long-term pooling.
                        I also provided reasonable parameter values which could work out in my opinion.

                        If you have any questions feel free to reach out to me.
                            I am really looking forward to your feedback and the discussion with you afterwards.
                            I hope that the project and the #degenhorde benefits from this long-term pooling reward concept."""
            ]
        , paragraph [ paddingXY 0 normal ]
            [ text "Thank you very much for reading :)"
            ]
        , paragraph []
            [ text """
                            Hint 1: the token balances are chronological and the graphs are antichronological
                            """
            ]
        , paragraph []
            [ text """
                            Hint 2: the red, yellow and green lines in the second graph are the weighted balances.
                            They are calculated by multiplying each weekly balance (dark red / dark yellow / dark green)
                            with the corresponding time weight (blue line).
                            The area under the curve of each weighted balance (red / yellow / green) is the airdropCredits function.
                            """
            ]
        , paragraph []
            [ text """
                            Hint 3: this tool is only for exploration and educational purposes to support the process of
                            designing a proper long-term pooling reward.
                            After the community and of course the team committed to one concrete approach we can implement
                            an automatic tracking tool connected to the Ethereum blockchain. So everybody can check his/her airdrop share at anytime.
                            I also already started to integrate the real (historical) LP token balancesfor automatic tracking by connecting to Etherscan.
                            """
            ]
        , paragraph []
            [ text """Hint 4: I only added two airdrop pools to keep the calculator simple,
                    the automatic tracking tool will support more airdrop pools."""
            ]
        , paragraph [ padding small ]
            []
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
            , viewDailyLPBalances DailyLPBalancesChanged "DGVC" model.dgvcLPBalances model.dgvcLPBalancesRaw
            , viewDailyLPBalances Airdrop1LPBalancesChanged "Airdrop 1" model.airdrop1LPBalances model.airdrop1LPBalancesRaw
            , viewDailyLPBalances Airdrop2LPBalancesChanged "Airdrop 2" model.airdrop2LPBalances model.airdrop2LPBalancesRaw
            , row [ width fill, spacing normal ]
                [ viewTimeCreditShift model.timeCreditShift model.timeCreditShiftRaw
                , viewTimeCreditStretch model.timeCreditStretch model.timeCreditStretchRaw

                --, viewDgvcShare model.dgvcShare model.dgvcShareRaw
                ]
            , wrappedRow [ width fill ]
                [ viewTimeCreditGraph model.timeCreditShift model.timeCreditStretch
                , viewTimeCreditBalances model.timeCreditShift model.timeCreditStretch model.dgvcLPBalances model.airdrop1LPBalances model.airdrop2LPBalances
                ]
            , viewResults model
            ]
