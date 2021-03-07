module Main exposing (Buffs, MemoryLevel, Model, getStatus, main, update, viewIdolPullDown)

import Browser
import Html exposing (..)
import Html.Attributes exposing (checked, class, selected, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra as Events
import Html.Extra as Html
import Idol
import Round



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { leader : FesIdol
    , vocalist : FesIdol
    , center : FesIdol
    , dancer : FesIdol
    , visualist : FesIdol
    , idolAppealParam : IdolAppealParam
    , buffs : Buffs
    , condition : Condition
    }


type alias FesIdol =
    { idol : Idol.Idol
    , vocal : Int
    , dance : Int
    , visual : Int
    , mental : Int
    , memoryLevel : MemoryLevel
    , gradAbilities : List GradAbility
    }


type alias IdolAppealParam =
    { idol : Idol.Idol
    , appealCoefficient : AppealCoefficient
    , vocal : Float
    , dance : Float
    , visual : Float
    , memoryCoefficient : Float
    }


type AppealCoefficient
    = Perfect
    | Good
    | Normal
    | Bad


type alias Buffs =
    { vocal : Int
    , dance : Int
    , visual : Int
    }


type ConditionType
    = TurnCount
    | MemoryGaugePercentage


type alias Condition =
    { turnCount : Int
    , memoryGaugePercentage : Int
    }


appealCoefficients : List AppealCoefficient
appealCoefficients =
    [ Perfect, Good, Normal, Bad ]


appealCoefficientToString : AppealCoefficient -> String
appealCoefficientToString appealCoefficient =
    case appealCoefficient of
        Perfect ->
            "Perfect"

        Good ->
            "Good"

        Normal ->
            "Normal"

        Bad ->
            "Bad"


toAppealCoefficient : String -> AppealCoefficient
toAppealCoefficient str =
    case str of
        "Perfect" ->
            Perfect

        "Good" ->
            Good

        "Normal" ->
            Normal

        "Bad" ->
            Bad

        --一致しない値が来たらPerfectへ設定
        _ ->
            Perfect


init : Model
init =
    { leader = FesIdol Idol.Meguru 500 500 500 300 One [ Slowstarter, Startdash, Favorite, AppealUp_theMoreMemoryGauge, AppealUp_theLessMemoryGauge ]
    , vocalist = FesIdol Idol.Hiori 500 500 500 300 One []
    , center = FesIdol Idol.Mano 500 500 500 300 One []
    , dancer = FesIdol Idol.Kogane 500 500 500 300 One []
    , visualist = FesIdol Idol.Kaho 500 500 500 300 One []
    , idolAppealParam = IdolAppealParam Idol.Mano Perfect 1.0 1.0 1.0 0.0
    , buffs = Buffs 0 0 0
    , condition = Condition 1 70
    }



-- UPDATE


type Msg
    = ChangeFesIdolStatus FesUnitPosition FesIdolStatus String
    | ChangeAppealer String
    | ChangeAppealCoefficient String
    | ChangeAppealPower AppealType String
    | ChangeMemoryAppealCoefficient String
    | ChangeBuff AppealType String
    | ChangeFesIdolAbility FesUnitPosition GradAbility
    | ChangeCondition ConditionType String



--TODO: アピールするアイドルがフェスユニットから外された時、アピール値が更新されない挙動を修正したい。


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeFesIdolStatus position status value ->
            updateFesIdol model position status value

        ChangeAppealer appealer ->
            let
                oldParam =
                    model.idolAppealParam

                newParam =
                    { oldParam | idol = Idol.fromString appealer }
            in
            { model | idolAppealParam = newParam }

        ChangeAppealCoefficient appealCoefficient ->
            let
                oldParam =
                    model.idolAppealParam

                newParam =
                    { oldParam | appealCoefficient = toAppealCoefficient appealCoefficient }
            in
            { model | idolAppealParam = newParam }

        ChangeAppealPower appealType power ->
            let
                oldParam =
                    model.idolAppealParam

                newParam =
                    case appealType of
                        Vo ->
                            { oldParam | vocal = String.toFloat power |> Maybe.withDefault 0.0 }

                        Da ->
                            { oldParam | dance = String.toFloat power |> Maybe.withDefault 0.0 }

                        Vi ->
                            { oldParam | visual = String.toFloat power |> Maybe.withDefault 0.0 }
            in
            { model | idolAppealParam = newParam }

        ChangeMemoryAppealCoefficient memoryCoefficient ->
            let
                oldParam =
                    model.idolAppealParam

                newParam =
                    { oldParam | memoryCoefficient = String.toFloat memoryCoefficient |> Maybe.withDefault 0.0 }
            in
            { model | idolAppealParam = newParam }

        ChangeBuff appealType newBuff ->
            let
                oldBuffs =
                    model.buffs

                newBuffs =
                    case appealType of
                        Vo ->
                            { oldBuffs | vocal = String.toInt newBuff |> Maybe.withDefault 0 }

                        Da ->
                            { oldBuffs | dance = String.toInt newBuff |> Maybe.withDefault 0 }

                        Vi ->
                            { oldBuffs | visual = String.toInt newBuff |> Maybe.withDefault 0 }
            in
            { model | buffs = newBuffs }

        ChangeFesIdolAbility position ability ->
            let
                oldIdol =
                    getFesIdol model position

                newIdol =
                    { oldIdol
                        | gradAbilities =
                            if List.member ability oldIdol.gradAbilities then
                                List.filter ((/=) ability) oldIdol.gradAbilities

                            else
                                ability :: oldIdol.gradAbilities
                    }
            in
            case position of
                Leader ->
                    updateLeader model newIdol

                Vocalist ->
                    updateVocalist model newIdol

                Center ->
                    updateCenter model newIdol

                Dancer ->
                    updateDancer model newIdol

                Visualist ->
                    updateVisualist model newIdol

        ChangeCondition conditionType value ->
            let
                oldCondition =
                    model.condition

                newCondition =
                    case conditionType of
                        TurnCount ->
                            { oldCondition | turnCount = value |> String.toInt |> Maybe.withDefault 1 }

                        MemoryGaugePercentage ->
                            { oldCondition | memoryGaugePercentage = value |> String.toInt |> Maybe.withDefault 70 }
            in
            { model | condition = newCondition }


updateFesIdol : Model -> FesUnitPosition -> FesIdolStatus -> String -> Model
updateFesIdol model position status value =
    let
        oldIdol =
            getFesIdol model position

        newIdol =
            case status of
                Idol ->
                    { oldIdol | idol = Idol.fromString value }

                Vocal ->
                    { oldIdol | vocal = String.toInt value |> Maybe.withDefault 0 }

                Dance ->
                    { oldIdol | dance = String.toInt value |> Maybe.withDefault 0 }

                Visual ->
                    { oldIdol | visual = String.toInt value |> Maybe.withDefault 0 }

                Mental ->
                    { oldIdol | mental = String.toInt value |> Maybe.withDefault 0 }

                MemoryLevel ->
                    { oldIdol | memoryLevel = fromString value }
    in
    case position of
        Leader ->
            updateLeader model newIdol

        Vocalist ->
            updateVocalist model newIdol

        Center ->
            updateCenter model newIdol

        Dancer ->
            updateDancer model newIdol

        Visualist ->
            updateVisualist model newIdol


updateLeader : Model -> FesIdol -> Model
updateLeader model newLeader =
    { model | leader = newLeader }


updateVocalist : Model -> FesIdol -> Model
updateVocalist model newVocalist =
    { model | vocalist = newVocalist }


updateCenter : Model -> FesIdol -> Model
updateCenter model newCenter =
    { model | center = newCenter }


updateDancer : Model -> FesIdol -> Model
updateDancer model newDancer =
    { model | dancer = newDancer }


updateVisualist : Model -> FesIdol -> Model
updateVisualist model newVisualist =
    { model | visualist = newVisualist }



-- VIEW


view : Model -> Html Msg
view model =
    let
        bonusAppliedModel =
            applyBonus model
    in
    div []
        [ div [ class "container" ]
            [ h1 [] [ text "フェスアピール値シミュレータ" ]
            ]
        , div [ class "container" ]
            [ viewJudgeArea bonusAppliedModel
            , viewBuffArea model
            , viewFesUnitArea model
            ]
        ]


applyBonus : Model -> Model
applyBonus rawModel =
    let
        bonusedLeader =
            applyBonusToFesIdol rawModel Leader

        bonusedVocalist =
            applyBonusToFesIdol rawModel Vocalist

        bonusedCenter =
            applyBonusToFesIdol rawModel Center

        bonusedDancer =
            applyBonusToFesIdol rawModel Dancer

        bonusedVisualist =
            applyBonusToFesIdol rawModel Visualist
    in
    Model bonusedLeader bonusedVocalist bonusedCenter bonusedDancer bonusedVisualist rawModel.idolAppealParam rawModel.buffs rawModel.condition


type alias StatusBonus =
    -- 各ステータスのボーナス値をパーセント表示で保持。
    { vocal : Int
    , dance : Int
    , visual : Int
    , mental : Int
    }


applyBonusToFesIdol : Model -> FesUnitPosition -> FesIdol
applyBonusToFesIdol rawModel unitPosition =
    let
        targetIdol =
            case unitPosition of
                Leader ->
                    rawModel.leader

                Vocalist ->
                    rawModel.vocalist

                Center ->
                    rawModel.center

                Dancer ->
                    rawModel.dancer

                Visualist ->
                    rawModel.visualist

        unitIdols =
            [ rawModel.leader.idol, rawModel.dancer.idol, rawModel.center.idol, rawModel.vocalist.idol, rawModel.visualist.idol ]

        existsSameUnit =
            List.filter ((/=) targetIdol.idol) unitIdols
                |> List.map Idol.whichUnit
                |> List.any ((==) (Idol.whichUnit targetIdol.idol))

        unitBonus =
            if existsSameUnit then
                StatusBonus 20 20 20 20

            else
                StatusBonus 0 0 0 0

        totalPositionBonus =
            calcTotalPositionBonus unitPosition targetIdol

        totalBonus =
            sum unitBonus totalPositionBonus
    in
    FesIdol
        targetIdol.idol
        (((targetIdol.vocal |> Basics.toFloat) * (1.0 + (totalBonus.vocal |> Basics.toFloat) * 0.01)) |> Round.roundNum 6 |> Basics.ceiling)
        (((targetIdol.dance |> Basics.toFloat) * (1.0 + (totalBonus.dance |> Basics.toFloat) * 0.01)) |> Round.roundNum 6 |> Basics.ceiling)
        (((targetIdol.visual |> Basics.toFloat) * (1.0 + (totalBonus.visual |> Basics.toFloat) * 0.01)) |> Round.roundNum 6 |> Basics.ceiling)
        (((targetIdol.mental |> Basics.toFloat) * (1.0 + (totalBonus.mental |> Basics.toFloat) * 0.01)) |> Round.roundNum 6 |> Basics.ceiling)
        targetIdol.memoryLevel
        targetIdol.gradAbilities


calcTotalPositionBonus : FesUnitPosition -> FesIdol -> StatusBonus
calcTotalPositionBonus unitPosition fesIdol =
    let
        basePositionBonus =
            case unitPosition of
                Leader ->
                    StatusBonus 0 0 0 100

                Vocalist ->
                    StatusBonus 100 0 0 0

                Center ->
                    StatusBonus 50 50 50 0

                Dancer ->
                    StatusBonus 0 100 0 0

                Visualist ->
                    StatusBonus 0 0 100 0

        gradBonus =
            calcGradPositionBonus unitPosition fesIdol
    in
    sum basePositionBonus gradBonus


sum : StatusBonus -> StatusBonus -> StatusBonus
sum statusBonus1 statusBonus2 =
    StatusBonus
        (statusBonus1.vocal + statusBonus2.vocal)
        (statusBonus1.dance + statusBonus2.dance)
        (statusBonus1.visual + statusBonus2.visual)
        (statusBonus1.mental + statusBonus2.mental)


calcGradPositionBonus : FesUnitPosition -> FesIdol -> StatusBonus
calcGradPositionBonus unitPosition fesIdol =
    case unitPosition of
        Leader ->
            StatusBonus 0 0 0 (calcGradPositionBonusPercentage Leader fesIdol.gradAbilities)

        Vocalist ->
            StatusBonus (calcGradPositionBonusPercentage Vocalist fesIdol.gradAbilities) 0 0 0

        Center ->
            StatusBonus
                (calcGradPositionBonusPercentage Center fesIdol.gradAbilities)
                (calcGradPositionBonusPercentage Center fesIdol.gradAbilities)
                (calcGradPositionBonusPercentage Center fesIdol.gradAbilities)
                0

        Dancer ->
            StatusBonus 0 (calcGradPositionBonusPercentage Dancer fesIdol.gradAbilities) 0 0

        Visualist ->
            StatusBonus 0 0 (calcGradPositionBonusPercentage Visualist fesIdol.gradAbilities) 0


calcGradPositionBonusPercentage : FesUnitPosition -> List GradAbility -> Int
calcGradPositionBonusPercentage position abilities =
    case position of
        Center ->
            List.foldr (+)
                0
                [ if List.member (Suitable1 Center) abilities then
                    5

                  else
                    0
                , if List.member (Suitable2 Center) abilities then
                    10

                  else
                    0
                , if List.member Suitable_all_1 abilities then
                    5

                  else
                    0
                , if List.member Suitable_all_2 abilities then
                    10

                  else
                    0
                ]

        other ->
            List.foldr (+)
                0
                [ if List.member (Suitable1 other) abilities then
                    10

                  else
                    0
                , if List.member (Suitable2 other) abilities then
                    15

                  else
                    0
                , if List.member Suitable_all_1 abilities then
                    5

                  else
                    0
                , if List.member Suitable_all_2 abilities then
                    10

                  else
                    0
                ]


viewJudgeArea : Model -> Html Msg
viewJudgeArea model =
    div [ class "container" ]
        [ table [ class "table" ]
            [ thead []
                [ tr []
                    [ th [] []
                    , th [ class "table-vocal" ] [ text "Voアピール" ]
                    , th [ class "table-dance" ] [ text "Daアピール" ]
                    , th [ class "table-visual" ] [ text "Viアピール" ]
                    , th [ class "table-info" ]
                        [ text "思い出アピール"
                        ]
                    ]
                , tr []
                    [ th [] []
                    , th [ class "table-vocal" ] [ viewAppealPower Vo model ]
                    , th [ class "table-dance" ] [ viewAppealPower Da model ]
                    , th [ class "table-visual" ] [ viewAppealPower Vi model ]
                    , th [ class "table-info" ] [ viewMemoryAppealPullDown model.idolAppealParam.memoryCoefficient ]
                    ]
                , tr []
                    [ th [] [ text "バフ合計" ]
                    , th [ class "table-vocal" ] [ viewBuffSlider Vo model ]
                    , th [ class "table-dance" ] [ viewBuffSlider Da model ]
                    , th [ class "table-visual" ] [ viewBuffSlider Vi model ]
                    , th [ class "table-info" ] []
                    ]
                ]
            , tbody []
                [ viewJudge Vo model
                , viewJudge Da model
                , viewJudge Vi model
                ]
            ]
        , ul [ class "list-group list-group-horizontal row" ]
            [ li [ class "list-group-item col-sm" ] [ text "アピールアイドル : ", viewAppealIdolPulldown model ]
            , li [ class "list-group-item col-sm" ] [ text "係数 : ", viewAppealCoefficient model ]
            ]
        , strong [] [ text ("合計G.R.A.D.バフ : " ++ (calcGradBuff model |> Round.round 1) ++ " %") ]
        , ul [ class "list-group list-group-horizontal row" ]
            [ li [ class "list-group-item col-sm" ] [ text "経過ターン数 : ", viewTurnSlider model, viewTurnInput model, text "ターン目" ] ]
        , ul [ class "list-group list-group-horizontal row" ]
            [ li [ class "list-group-item col-sm" ] [ text "思い出ゲージ : ", viewMemoryGaugeSlider model, viewMemoryGaugeInput model, text "%" ] ]
        ]



{-
   viewJudge : 審査員へのアピール値の計算・表示をするエリア。
-}


viewJudge : AppealType -> Model -> Html msg
viewJudge judgeType model =
    let
        cssClass =
            case judgeType of
                Vo ->
                    class "table-vocal"

                Da ->
                    class "table-dance"

                Vi ->
                    class "table-visual"
    in
    tr []
        [ th [ cssClass ] [ text (typeHeader judgeType ++ "審査員") ]
        , td [] [ text (String.fromInt (calcNormalAppeal model Vo judgeType)) ]
        , td [] [ text (String.fromInt (calcNormalAppeal model Da judgeType)) ]
        , td [] [ text (String.fromInt (calcNormalAppeal model Vi judgeType)) ]
        , td [] [ text (String.fromInt (calcMemoryAppeal model judgeType)) ]
        ]


calcNormalAppeal : Model -> AppealType -> AppealType -> Int
calcNormalAppeal model appealType judgeType =
    floor
        (basicCoefficent model appealType (toFloat model.idolAppealParam.appealCoefficient)
            * appealPower model appealType
        )
        * calcJudgeBuff appealType judgeType


calcJudgeBuff : AppealType -> AppealType -> Int
calcJudgeBuff appealType judgeType =
    if appealType == judgeType then
        2

    else
        1


calcMemoryAppeal : Model -> AppealType -> Int
calcMemoryAppeal model judgeType =
    memoryAppealBase model judgeType model.idolAppealParam.memoryCoefficient


memoryAppealBase : Model -> AppealType -> Float -> Int
memoryAppealBase model judgeType memoryCo =
    let
        appealTypes =
            [ Vo, Da, Vi ]

        judgeBuffs =
            List.map2 calcJudgeBuff appealTypes (List.repeat 3 judgeType)

        unitIdolsMemoryLv =
            [ model.leader.memoryLevel
            , model.dancer.memoryLevel
            , model.vocalist.memoryLevel
            , model.visualist.memoryLevel
            ]

        unitBuff =
            1.0 + (List.map convertToUnitBuff unitIdolsMemoryLv |> List.sum)
    in
    -- 各属性のjudgeへの思い出アピール値を計算する
    List.map2 (basicCoefficent model) appealTypes (List.repeat 3 memoryCo)
        |> List.map2 (*) (List.repeat 3 (convertToMemoryPower model.center.memoryLevel))
        |> List.map Basics.floor
        |> List.map Basics.toFloat
        |> List.map2 (*) (List.repeat 3 unitBuff)
        |> List.map Basics.floor
        |> List.map2 (*) judgeBuffs
        |> List.sum


appealPower : Model -> AppealType -> Float
appealPower model appealType =
    case appealType of
        Vo ->
            model.idolAppealParam.vocal

        Da ->
            model.idolAppealParam.dance

        Vi ->
            model.idolAppealParam.visual



-- アピール基礎係数を計算する


basicCoefficent : Model -> AppealType -> Float -> Float
basicCoefficent model appealType appealCoefficient =
    floor
        (fesAppealBase model appealType
            * (1.0 + (calcTotalBuff model appealType / 100.0))
            * appealCoefficient
        )
        |> Basics.toFloat



-- フェスアピール基礎値を計算する


fesAppealBase : Model -> AppealType -> Float
fesAppealBase model appealType =
    let
        unitIdols =
            [ model.leader, model.dancer, model.center, model.vocalist, model.visualist ]

        appealer =
            case List.filter (sameIdol model.idolAppealParam.idol) unitIdols of
                idol :: _ ->
                    idol

                [] ->
                    model.center

        appealerStatus =
            getStatus appealer (typeToStatus appealType) |> String.toFloat |> Maybe.withDefault 0

        notAppealers =
            List.filter ((/=) appealer) unitIdols

        statusSumOfNotAppealers =
            List.map2 getStatus notAppealers (List.repeat (List.length notAppealers) (typeToStatus appealType)) |> List.filterMap String.toInt |> List.sum |> Basics.toFloat
    in
    floor (2.0 * appealerStatus + 0.5 * statusSumOfNotAppealers) |> Basics.toFloat


sameIdol : Idol.Idol -> FesIdol -> Bool
sameIdol idol fesIdol =
    idol == fesIdol.idol


toFloat : AppealCoefficient -> Float
toFloat appealCoefficient =
    case appealCoefficient of
        Perfect ->
            1.5

        Good ->
            1.1

        Normal ->
            1.0

        Bad ->
            0.5


type AppealType
    = Vo
    | Da
    | Vi


typeHeader : AppealType -> String
typeHeader appealType =
    case appealType of
        Vo ->
            "Vo"

        Da ->
            "Da"

        Vi ->
            "Vi"


calcTotalBuff : Model -> AppealType -> Float
calcTotalBuff model buffType =
    case buffType of
        Vo ->
            (model.buffs.vocal |> Basics.toFloat) + calcGradBuff model

        Da ->
            (model.buffs.dance |> Basics.toFloat) + calcGradBuff model

        Vi ->
            (model.buffs.visual |> Basics.toFloat) + calcGradBuff model


calcGradBuff : Model -> Float
calcGradBuff model =
    let
        gradAbilities =
            [ Startdash, Slowstarter, AppealUp_theMoreMemoryGauge, AppealUp_theLessMemoryGauge ]

        gradAbilitiesBuff =
            List.map2 calcGradAbiiltyBuff gradAbilities (List.repeat (List.length gradAbilities) model.condition)

        gradAbilitiesCount =
            List.map2 countAbility (List.repeat (List.length gradAbilities) model) gradAbilities
                |> List.map Basics.toFloat

        totalBondsBuff =
            (List.map BondsWith [ model.leader.idol, model.vocalist.idol, model.center.idol, model.dancer.idol, model.visualist.idol ]
                |> List.map (countAbility model)
                |> List.sum
                |> Basics.toFloat
            )
                * 5.0
    in
    (List.map2 (*) gradAbilitiesBuff gradAbilitiesCount |> List.sum) + totalBondsBuff



{-
   viewBuffArea : Buffの値を設定するエリア
-}


viewBuffArea : Model -> Html Msg
viewBuffArea model =
    div [ class "container" ]
        [ h2 [] [ text "G.R.A.D.バフ" ]
        , table [ class "table table-sm" ]
            [ thead []
                [ tr []
                    [ th [] [ text "アビリティ" ]
                    , th [] [ text "所持数" ]
                    , th [] [ text "バフ合計" ]
                    ]
                ]
            , tbody []
                ([ tr []
                    [ td [] [ text (gradAbilityToString Startdash) ]
                    , td [] [ text (countAbility model Startdash |> String.fromInt) ]
                    , td [] [ text (((calcGradAbiiltyBuff Startdash model.condition * Basics.toFloat (countAbility model Startdash)) |> Round.round 1) ++ "%") ]
                    ]
                 , tr []
                    [ td [] [ text (gradAbilityToString Slowstarter) ]
                    , td [] [ text (countAbility model Slowstarter |> String.fromInt) ]
                    , td [] [ text (((calcGradAbiiltyBuff Slowstarter model.condition * Basics.toFloat (countAbility model Slowstarter)) |> Round.round 1) ++ "%") ]
                    ]
                 , tr []
                    [ td [] [ text (gradAbilityToString AppealUp_theMoreMemoryGauge) ]
                    , td [] [ text (countAbility model AppealUp_theMoreMemoryGauge |> String.fromInt) ]
                    , td [] [ text (((calcGradAbiiltyBuff AppealUp_theMoreMemoryGauge model.condition * Basics.toFloat (countAbility model AppealUp_theMoreMemoryGauge)) |> Round.round 1) ++ "%") ]
                    ]
                 , tr []
                    [ td [] [ text (gradAbilityToString AppealUp_theLessMemoryGauge) ]
                    , td [] [ text (countAbility model AppealUp_theLessMemoryGauge |> String.fromInt) ]
                    , td [] [ text (((calcGradAbiiltyBuff AppealUp_theLessMemoryGauge model.condition * Basics.toFloat (countAbility model AppealUp_theLessMemoryGauge)) |> Round.round 1) ++ "%") ]
                    ]
                 ]
                    ++ viewBondsArea model
                )
            ]
        ]


countAbility : Model -> GradAbility -> Int
countAbility model ability =
    (model.leader.gradAbilities ++ model.vocalist.gradAbilities ++ model.center.gradAbilities ++ model.dancer.gradAbilities ++ model.visualist.gradAbilities)
        |> List.filter ((==) ability)
        |> List.length


calcGradAbiiltyBuff : GradAbility -> Condition -> Float
calcGradAbiiltyBuff ability condition =
    case ability of
        Startdash ->
            if condition.turnCount > 0 && condition.turnCount <= 10 then
                4.0 + 16.0 / 9.0 * Basics.toFloat (condition.turnCount - 1)

            else if condition.turnCount > 10 then
                20.0

            else
                0.0

        Slowstarter ->
            if condition.turnCount > 0 && condition.turnCount <= 10 then
                10.0 - 8.0 / 9.0 * Basics.toFloat (condition.turnCount - 1)

            else if condition.turnCount > 10 then
                2.0

            else
                0

        AppealUp_theMoreMemoryGauge ->
            if condition.memoryGaugePercentage >= 0 && condition.memoryGaugePercentage <= 100 then
                2.0 + 8.0 * Basics.toFloat condition.memoryGaugePercentage / 100.0

            else
                0

        AppealUp_theLessMemoryGauge ->
            if condition.memoryGaugePercentage >= 0 && condition.memoryGaugePercentage <= 100 then
                20.0 - 16.0 * Basics.toFloat condition.memoryGaugePercentage / 100.0

            else
                0

        _ ->
            0


viewBondsArea : Model -> List (Html msg)
viewBondsArea model =
    [ viewBondsBuffRow model model.leader.idol
    , viewBondsBuffRow model model.vocalist.idol
    , viewBondsBuffRow model model.center.idol
    , viewBondsBuffRow model model.dancer.idol
    , viewBondsBuffRow model model.visualist.idol
    ]


viewBondsBuffRow : Model -> Idol.Idol -> Html msg
viewBondsBuffRow model idol =
    tr []
        [ td [] [ text (Idol.toFullName idol ++ "との絆") ]
        , td [] [ text (String.fromInt (countAbility model (BondsWith idol))) ]
        , td [] [ text (((countAbility model (BondsWith idol) * 5) |> Basics.toFloat |> Round.round 1) ++ "%") ]
        ]


viewTurnSlider : Model -> Html Msg
viewTurnSlider model =
    input
        [ type_ "range"
        , Html.Attributes.min "1"
        , Html.Attributes.max "10"
        , step "1"
        , value (model.condition.turnCount |> String.fromInt)
        , onInput (ChangeCondition TurnCount)
        ]
        []


viewTurnInput : Model -> Html Msg
viewTurnInput model =
    input [ style "width" "4em", value (model.condition.turnCount |> String.fromInt), onInput (ChangeCondition TurnCount) ] []


viewMemoryGaugeSlider : Model -> Html Msg
viewMemoryGaugeSlider model =
    input
        [ type_ "range"
        , Html.Attributes.min "0"
        , Html.Attributes.max "100"
        , step "5"
        , value (model.condition.memoryGaugePercentage |> String.fromInt)
        , onInput (ChangeCondition MemoryGaugePercentage)
        ]
        []


viewMemoryGaugeInput : Model -> Html Msg
viewMemoryGaugeInput model =
    input
        [ style "width" "4em"
        , value (model.condition.memoryGaugePercentage |> String.fromInt)
        , onInput (ChangeCondition MemoryGaugePercentage)
        ]
        []


viewBuffSlider : AppealType -> Model -> Html Msg
viewBuffSlider appealType model =
    div []
        [ input
            [ type_ "range"
            , Html.Attributes.min "-100"
            , Html.Attributes.max "999"
            , step "1"
            , value (getBuff model.buffs appealType |> String.fromInt)
            , onInput (ChangeBuff appealType)
            ]
            []
        , input [ style "width" "4em", value (getBuff model.buffs appealType |> String.fromInt), onInput (ChangeBuff appealType) ] []
        , text "%"
        ]


getBuff : Buffs -> AppealType -> Int
getBuff buffs appealType =
    case appealType of
        Vo ->
            buffs.vocal

        Da ->
            buffs.dance

        Vi ->
            buffs.visual



{-
   viewAppealArea : アピールするアイドル、PerFect/Good/Normal/Bad、Vo/Da/Vi倍率を選択するエリア
-}


viewAppealIdolPulldown : Model -> Html Msg
viewAppealIdolPulldown model =
    -- フェスユニットのアイドルを選択肢に表示するプルダウンを表示
    select
        [ Events.onChange ChangeAppealer ]
        --(List.map (viewMemoryLevelOption (getStatus fesIdol MemoryLevel)) (List.range 0 5))
        (List.map (viewIdolOption (Idol.toString model.idolAppealParam.idol)) (listFesUnitMember model))


listFesUnitMember : Model -> List Idol.Idol
listFesUnitMember model =
    [ model.leader.idol, model.vocalist.idol, model.center.idol, model.dancer.idol, model.visualist.idol ]


viewAppealCoefficient : Model -> Html Msg
viewAppealCoefficient model =
    select
        [ Events.onChange ChangeAppealCoefficient ]
        (List.map (viewAppealCoefficientOption (appealCoefficientToString model.idolAppealParam.appealCoefficient)) appealCoefficients)


viewAppealCoefficientOption : String -> AppealCoefficient -> Html Msg
viewAppealCoefficientOption selectedCoefficient appealCoefficient =
    option
        [ selected (appealCoefficientToString appealCoefficient == selectedCoefficient), value (appealCoefficientToString appealCoefficient) ]
        [ text (appealCoefficientToString appealCoefficient) ]


viewAppealPower : AppealType -> Model -> Html Msg
viewAppealPower appealType model =
    div []
        [ input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "10"
            , step "0.1"
            , value (appealPower model appealType |> String.fromFloat)
            , onInput (ChangeAppealPower appealType)
            ]
            []
        , input [ style "width" "4em", value (appealPower model appealType |> String.fromFloat), onInput (ChangeAppealPower appealType) ] []
        , text "倍"
        ]


viewMemoryAppealPullDown : Float -> Html Msg
viewMemoryAppealPullDown memoryAppealCoefficient =
    select
        [ Events.onChange ChangeMemoryAppealCoefficient ]
        [ option [ selected (memoryAppealCoefficient == 0), value "0" ] [ text "なし" ]
        , option [ selected (memoryAppealCoefficient == 0.5), value "0.5" ] [ text "Bad" ]
        , option [ selected (memoryAppealCoefficient == 1.5), value "1.5" ] [ text "Good" ]
        ]



{-
   viewBonusedFesUnitArea : ボーナス適用後のステータスを表示するエリア
-}
{-
   viewFesUnitArea : フェスユニット編成、ステータス設定を行うエリア
-}


viewFesUnitArea : Model -> Html Msg
viewFesUnitArea model =
    div [ class "container" ]
        [ h2 [] [ text "フェスユニットステータス" ]
        , table [ class "table table-sm" ]
            [ thead []
                [ tr []
                    [ th [] []
                    , th [] [ text "Leader" ]
                    , th [] [ text "Vocal担当" ]
                    , th [] [ text "Center" ]
                    , th [] [ text "Dance担当" ]
                    , th [] [ text "Visual担当" ]
                    ]
                ]
            , tbody []
                [ viewFesIdol model
                , viewFesIdolStatus model Vocal
                , viewFesIdolStatus model Dance
                , viewFesIdolStatus model Visual
                , viewFesIdolStatus model Mental
                , viewFesIdolMemoryLevel model
                , viewGradAbilities model
                ]
            ]
        ]


viewFesIdolStatus : Model -> FesIdolStatus -> Html Msg
viewFesIdolStatus model status =
    let
        rowClass =
            case status of
                Vocal ->
                    class "table-vocal"

                Dance ->
                    class "table-dance"

                Visual ->
                    class "table-visual"

                Mental ->
                    class "table-mental"

                _ ->
                    class "table-light"

        bonusAppliedModel =
            applyBonus model
    in
    tr [ rowClass ]
        [ th [] [ text (statusHeader status) ]
        , td [ class "fw-bold" ]
            [ input [ style "width" "4em", value (getStatus (getFesIdol model Leader) status), onInput (ChangeFesIdolStatus Leader status) ] []
            , text " → "
            , text (getStatus bonusAppliedModel.leader status)
            ]
        , td [ class "fw-bold" ]
            [ input [ style "width" "4em", value (getStatus (getFesIdol model Vocalist) status), onInput (ChangeFesIdolStatus Vocalist status) ] []
            , text " → "
            , text (getStatus bonusAppliedModel.vocalist status)
            ]
        , td [ class "fw-bold" ]
            [ input [ style "width" "4em", value (getStatus (getFesIdol model Center) status), onInput (ChangeFesIdolStatus Center status) ] []
            , text " → "
            , text (getStatus bonusAppliedModel.center status)
            ]
        , td [ class "fw-bold" ]
            [ input [ style "width" "4em", value (getStatus (getFesIdol model Dancer) status), onInput (ChangeFesIdolStatus Dancer status) ] []
            , text " → "
            , text (getStatus bonusAppliedModel.dancer status)
            ]
        , td [ class "fw-bold" ]
            [ input [ style "width" "4em", value (getStatus (getFesIdol model Visualist) status), onInput (ChangeFesIdolStatus Visualist status) ] []
            , text " → "
            , text (getStatus bonusAppliedModel.visualist status)
            ]
        ]


viewFesIdolMemoryLevel : Model -> Html Msg
viewFesIdolMemoryLevel model =
    tr []
        [ th [] [ text (statusHeader MemoryLevel) ]
        , td [] [ viewMemoryLevelPullDown (getFesIdol model Leader) Leader ]
        , td [] [ viewMemoryLevelPullDown (getFesIdol model Vocalist) Vocalist ]
        , td [] [ viewMemoryLevelPullDown (getFesIdol model Center) Center ]
        , td [] [ viewMemoryLevelPullDown (getFesIdol model Dancer) Dancer ]
        , td [] [ viewMemoryLevelPullDown (getFesIdol model Visualist) Visualist ]
        ]


viewMemoryLevelPullDown : FesIdol -> FesUnitPosition -> Html Msg
viewMemoryLevelPullDown fesIdol position =
    let
        memoryLevels =
            [ Zero, One, Two, Three, Four, Five ]
    in
    select
        [ Events.onChange (ChangeFesIdolStatus position MemoryLevel) ]
        (List.map (viewMemoryLevelOption (getStatus fesIdol MemoryLevel)) memoryLevels)


viewMemoryLevelOption : String -> MemoryLevel -> Html Msg
viewMemoryLevelOption selectedLevel memoryLevel =
    option
        [ selected (selectedLevel == (memoryLevel |> convertToInt |> String.fromInt))
        , value (memoryLevel |> convertToInt |> String.fromInt)
        ]
        [ text (memoryLevel |> convertToInt |> String.fromInt) ]


viewGradAbilities : Model -> Html Msg
viewGradAbilities model =
    tr []
        [ th [] [ text "G.R.A.D.アビリティ" ]
        , td [] [ viewGradAbilitiesOf Leader model.leader.gradAbilities ]
        , td [] [ viewGradAbilitiesOf Vocalist model.vocalist.gradAbilities ]
        , td [] [ viewGradAbilitiesOf Center model.center.gradAbilities ]
        , td [] [ viewGradAbilitiesOf Dancer model.dancer.gradAbilities ]
        , td [] [ viewGradAbilitiesOf Visualist model.visualist.gradAbilities ]
        ]


viewGradAbilitiesOf : FesUnitPosition -> List GradAbility -> Html Msg
viewGradAbilitiesOf position gradAbilities =
    let
        allAbilities =
            (List.map suitable1and2 [ Leader, Vocalist, Center, Dancer, Visualist ] |> List.concat)
                ++ [ Suitable_all_1
                   , Suitable_all_2
                   , Slowstarter
                   , Startdash
                   , Favorite -- 人気者
                   , Calm -- 物静か

                   --    , Spotlighted -- 注目の的
                   --    , Humble -- ひかえめ
                   , Perfectly
                   , AppealUp_theMoreMemoryGauge
                   , AppealUp_theLessMemoryGauge
                   ]
                ++ List.map BondsWith Idol.idols
    in
    div [] (List.map (viewAbility position gradAbilities) allAbilities)


viewAbility : FesUnitPosition -> List GradAbility -> GradAbility -> Html Msg
viewAbility position selectedAbilities abilityType =
    div []
        [ input [ type_ "checkbox", onClick (ChangeFesIdolAbility position abilityType), checked (List.member abilityType selectedAbilities) ] []
        , text (gradAbilityToString abilityType)
        ]


viewFesIdol : Model -> Html Msg
viewFesIdol model =
    tr [ class "table-info" ]
        [ th [] [ text (statusHeader Idol) ]
        , td [] [ viewIdolPullDown (getFesIdol model Leader) Leader ]
        , td [] [ viewIdolPullDown (getFesIdol model Vocalist) Vocalist ]
        , td [] [ viewIdolPullDown (getFesIdol model Center) Center ]
        , td [] [ viewIdolPullDown (getFesIdol model Dancer) Dancer ]
        , td [] [ viewIdolPullDown (getFesIdol model Visualist) Visualist ]
        ]


viewIdolPullDown : FesIdol -> FesUnitPosition -> Html Msg
viewIdolPullDown fesIdol position =
    select
        [ Events.onChange (ChangeFesIdolStatus position Idol) ]
        (List.map (viewIdolOption (getStatus fesIdol Idol)) Idol.idols)


viewIdolOption : String -> Idol.Idol -> Html Msg
viewIdolOption selectedIdol idol =
    option
        [ selected (Idol.toString idol == selectedIdol), value (Idol.toString idol) ]
        [ text (Idol.toString idol) ]


getFesIdol : Model -> FesUnitPosition -> FesIdol
getFesIdol model position =
    case position of
        Leader ->
            model.leader

        Vocalist ->
            model.vocalist

        Center ->
            model.center

        Dancer ->
            model.dancer

        Visualist ->
            model.visualist


getStatus : FesIdol -> FesIdolStatus -> String
getStatus fesIdol status =
    case status of
        Idol ->
            Idol.toString fesIdol.idol

        Vocal ->
            String.fromInt fesIdol.vocal

        Dance ->
            String.fromInt fesIdol.dance

        Visual ->
            String.fromInt fesIdol.visual

        Mental ->
            String.fromInt fesIdol.mental

        MemoryLevel ->
            fesIdol.memoryLevel |> convertToInt |> String.fromInt


typeToStatus : AppealType -> FesIdolStatus
typeToStatus appealType =
    case appealType of
        Vo ->
            Vocal

        Da ->
            Dance

        Vi ->
            Visual


statusHeader : FesIdolStatus -> String
statusHeader status =
    case status of
        Idol ->
            "アイドル"

        Vocal ->
            "Vo"

        Dance ->
            "Da"

        Visual ->
            "Vi"

        Mental ->
            "Me"

        MemoryLevel ->
            "思い出Lv"



--- type


type FesUnitPosition
    = Leader
    | Vocalist
    | Center
    | Dancer
    | Visualist


type FesIdolStatus
    = Idol
    | Vocal
    | Dance
    | Visual
    | Mental
    | MemoryLevel


type MemoryLevel
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five


convertToInt : MemoryLevel -> Int
convertToInt memoryLevel =
    case memoryLevel of
        Zero ->
            0

        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5


fromString : String -> MemoryLevel
fromString str =
    case str of
        "0" ->
            Zero

        "1" ->
            One

        "2" ->
            Two

        "3" ->
            Three

        "4" ->
            Four

        "5" ->
            Five

        _ ->
            Zero


convertToMemoryPower : MemoryLevel -> Float
convertToMemoryPower memoryLevel =
    case memoryLevel of
        Zero ->
            0.0

        One ->
            0.8

        Two ->
            1.0

        Three ->
            1.2

        Four ->
            1.4

        Five ->
            2.0


convertToUnitBuff : MemoryLevel -> Float
convertToUnitBuff memoryLevel =
    case memoryLevel of
        Zero ->
            0.0

        One ->
            0.0

        Two ->
            0.02

        Three ->
            0.03

        Four ->
            0.05

        Five ->
            0.075


type GradAbility
    = Suitable1 FesUnitPosition
    | Suitable2 FesUnitPosition
    | Suitable_all_1
    | Suitable_all_2
      -- | WeakMentality
      -- | StrongMentality
      -- | MemoryGauge_plusplus
      -- | MemoryGauge_plus
      -- | MemoryGauge_minus
      -- | Remove_melancholy_1
      -- | Remove_melancholy_2
      -- | Remove_relax_1
      -- | Remove_relax_2
      -- | RaiseLimit_plus_vocal
      -- | RaiseLimit_plusplus_vocal
      -- | RaiseLimit_plus_dance
      -- | RaiseLimit_plusplus_dance
      -- | RaiseLimit_plus_visual
      -- | RaiseLimit_plusplus_visual
      -- | RaiseLimit_plus_mental
      -- | RaiseLimit_plusplus_mental
      -- | Master AppealType
      -- | MentalRecovery_plus
      -- | MentalRecovery_minus
    | Slowstarter
    | Startdash
    | Favorite -- 人気者
    | Calm -- 物静か
      -- | Spotlighted -- 注目の的
      -- | Humble -- ひかえめ
    | Perfectly
    | AppealUp_theMoreMemoryGauge
    | AppealUp_theLessMemoryGauge
    | BondsWith Idol.Idol


gradAbilityToString : GradAbility -> String
gradAbilityToString ability =
    case ability of
        Suitable1 unitPosition ->
            unitPositionToString unitPosition ++ "適正◯"

        Suitable2 unitPosition ->
            unitPositionToString unitPosition ++ "適正◎"

        Suitable_all_1 ->
            "オールラウンダー◯"

        Suitable_all_2 ->
            "オールラウンダー◎"

        Slowstarter ->
            "スロースターター"

        Startdash ->
            "スタートダッシュ"

        Favorite ->
            "人気者"

        Calm ->
            "物静か"

        -- Spotlighted ->
        --     "注目の的"
        -- Humble ->
        --     "ひかえめ"
        Perfectly ->
            "パーフェクトリィ"

        AppealUp_theMoreMemoryGauge ->
            "アピールUP（思い出高）"

        AppealUp_theLessMemoryGauge ->
            "アピールUP（思い出低）"

        BondsWith idol ->
            Idol.toFullName idol ++ "との絆"


suitable1and2 : FesUnitPosition -> List GradAbility
suitable1and2 position =
    [ Suitable1 position, Suitable2 position ]


unitPositionToString : FesUnitPosition -> String
unitPositionToString position =
    case position of
        Leader ->
            "Leader"

        Vocalist ->
            "Vocal"

        Center ->
            "Center"

        Dancer ->
            "Dance"

        Visualist ->
            "Visual"
