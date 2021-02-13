module Main exposing (MemoryLevel, Model, getStatus, main, update, viewIdolPullDown)

import Browser
import Html exposing (..)
import Html.Attributes exposing (checked, max, min, selected, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra as Events
import Html.Extra as Html
import Idol



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
    { leader = FesIdol Idol.Meguru 500 500 500 300 One []
    , vocalist = FesIdol Idol.Hiori 500 500 500 300 One []
    , center = FesIdol Idol.Mano 500 500 500 300 One []
    , dancer = FesIdol Idol.Kogane 500 500 500 300 One []
    , visualist = FesIdol Idol.Kaho 500 500 500 300 One []
    , idolAppealParam = IdolAppealParam Idol.Mano Perfect 1.0 1.0 1.0 0.0
    , buffs = Buffs 0 0 0
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


updateIdolAppealParam : Model -> IdolAppealParam -> Model
updateIdolAppealParam model newIdolAppealParam =
    { model | idolAppealParam = newIdolAppealParam }



-- VIEW


view : Model -> Html Msg
view model =
    let
        bonusAppliedModel =
            applyBonus model
    in
    div []
        [ div []
            [ h1 [] [ text "フェスアピール値シミュレータ" ]
            ]
        , div []
            [ viewJudgeArea bonusAppliedModel
            , viewBuffArea model
            , viewAppealArea model
            , viewBonusedFesUnitArea bonusAppliedModel
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
    Model bonusedLeader bonusedVocalist bonusedCenter bonusedDancer bonusedVisualist rawModel.idolAppealParam rawModel.buffs


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
        (((targetIdol.vocal |> Basics.toFloat) * (1.0 + (totalBonus.vocal |> Basics.toFloat) * 0.01)) |> Basics.ceiling)
        (((targetIdol.dance |> Basics.toFloat) * (1.0 + (totalBonus.dance |> Basics.toFloat) * 0.01)) |> Basics.ceiling)
        (((targetIdol.visual |> Basics.toFloat) * (1.0 + (totalBonus.visual |> Basics.toFloat) * 0.01)) |> Basics.ceiling)
        (((targetIdol.mental |> Basics.toFloat) * (1.0 + (totalBonus.mental |> Basics.toFloat) * 0.01)) |> Basics.ceiling)
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
    --TODO: G.R.A.D.アビリティについては未反映
    StatusBonus 0 0 0 0


viewJudgeArea : Model -> Html msg
viewJudgeArea model =
    div []
        [ h2 [] [ text "アピール値" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "審査員タイプ" ]
                    , th [] [ text "Voアピール" ]
                    , th [] [ text "Daアピール" ]
                    , th [] [ text "Viアピール" ]
                    , th [] [ text "思い出アピール" ]
                    ]
                ]
            , tbody []
                [ viewJudge Vo model
                , viewJudge Da model
                , viewJudge Vi model
                ]
            ]
        ]



{-
   viewJudge : 審査員へのアピール値の計算・表示をするエリア。
-}


viewJudge : AppealType -> Model -> Html msg
viewJudge judgeType model =
    tr []
        [ td [] [ text (typeHeader judgeType) ]
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
            * (1 + Basics.toFloat (calcTotalBuff model appealType) / 100)
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


calcTotalBuff : Model -> AppealType -> Basics.Int
calcTotalBuff model buffType =
    case buffType of
        Vo ->
            model.buffs.vocal + calcGradBuff model

        Da ->
            model.buffs.dance + calcGradBuff model

        Vi ->
            model.buffs.visual + calcGradBuff model


calcGradBuff : Model -> Int
calcGradBuff model =
    --TODO: G.R.A.D.アビリティのバフ集計処理を作成する。
    0



{-
   viewBuffArea : Buffの値を設定するエリア
-}


viewBuffArea : Model -> Html Msg
viewBuffArea model =
    div []
        [ h2 [] [ text "バフ指定エリア" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Vocalバフ" ]
                    , th [] [ text "Danceバフ" ]
                    , th [] [ text "Visualバフ" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ viewBuffSlider Vo model ]
                    , td [] [ viewBuffSlider Da model ]
                    , td [] [ viewBuffSlider Vi model ]
                    ]
                ]
            ]
        ]


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


viewAppealArea : Model -> Html Msg
viewAppealArea model =
    div []
        [ h2 [] [ text "アピール倍率指定エリア" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "アピールするアイドル" ]
                    , th [] [ text "アピール係数" ]
                    , th [] [ text "Vo倍率" ]
                    , th [] [ text "Da倍率" ]
                    , th [] [ text "Vi倍率" ]
                    , th [] [ text "思い出アピール" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ viewAppealIdolPulldown model ]
                    , td [] [ viewAppealCoefficient model ]
                    , td [] [ viewAppealPower Vo model ]
                    , td [] [ viewAppealPower Da model ]
                    , td [] [ viewAppealPower Vi model ]
                    , td [] [ viewMemoryAppeal model ]
                    ]
                ]
            ]
        ]


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
        ]


viewMemoryAppeal : Model -> Html Msg
viewMemoryAppeal model =
    div []
        [ viewMemoryAppealPullDown model.idolAppealParam.memoryCoefficient ]


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


viewBonusedFesUnitArea : Model -> Html Msg
viewBonusedFesUnitArea model =
    div []
        [ h2 [] [ text "ボーナス適用後のステータス" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "ポジション" ]
                    , th [] [ text "Leader" ]
                    , th [] [ text "Vocal担当" ]
                    , th [] [ text "Center" ]
                    , th [] [ text "Dance担当" ]
                    , th [] [ text "Visual担当" ]
                    ]
                ]
            , tbody []
                [ writeFesIdol model
                , writeFesIdolStatus model Vocal
                , writeFesIdolStatus model Dance
                , writeFesIdolStatus model Visual
                , writeFesIdolStatus model Mental
                ]
            ]
        ]



{-
   viewFesUnitArea : フェスユニット編成、ステータス設定を行うエリア
-}


viewFesUnitArea : Model -> Html Msg
viewFesUnitArea model =
    div
        []
        [ h2 [] [ text "フェスユニットのステータス指定エリア" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "ポジション" ]
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
    tr []
        [ td [] [ text (statusHeader status) ]
        , td [] [ input [ style "width" "4em", value (getStatus (getFesIdol model Leader) status), onInput (ChangeFesIdolStatus Leader status) ] [] ]
        , td [] [ input [ style "width" "4em", value (getStatus (getFesIdol model Vocalist) status), onInput (ChangeFesIdolStatus Vocalist status) ] [] ]
        , td [] [ input [ style "width" "4em", value (getStatus (getFesIdol model Center) status), onInput (ChangeFesIdolStatus Center status) ] [] ]
        , td [] [ input [ style "width" "4em", value (getStatus (getFesIdol model Dancer) status), onInput (ChangeFesIdolStatus Dancer status) ] [] ]
        , td [] [ input [ style "width" "4em", value (getStatus (getFesIdol model Visualist) status), onInput (ChangeFesIdolStatus Visualist status) ] [] ]
        ]


writeFesIdolStatus : Model -> FesIdolStatus -> Html msg
writeFesIdolStatus model status =
    tr []
        [ td [] [ text (statusHeader status) ]
        , td [] [ text (getStatus model.leader status) ]
        , td [] [ text (getStatus model.vocalist status) ]
        , td [] [ text (getStatus model.center status) ]
        , td [] [ text (getStatus model.dancer status) ]
        , td [] [ text (getStatus model.visualist status) ]
        ]


viewFesIdolMemoryLevel : Model -> Html Msg
viewFesIdolMemoryLevel model =
    tr []
        [ td [] [ text (statusHeader MemoryLevel) ]
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
        [ td [] [ text "G.R.A.D.アビリティ" ]
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
                   , Spotlighted -- 注目の的
                   , Humble -- ひかえめ
                   , Perfectly
                   , AppealUp_theMoreMemoryGage
                   , AppealUp_theLessMemoryGage
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
    tr []
        [ td [] [ text (statusHeader Idol) ]
        , td [] [ viewIdolPullDown (getFesIdol model Leader) Leader ]
        , td [] [ viewIdolPullDown (getFesIdol model Vocalist) Vocalist ]
        , td [] [ viewIdolPullDown (getFesIdol model Center) Center ]
        , td [] [ viewIdolPullDown (getFesIdol model Dancer) Dancer ]
        , td [] [ viewIdolPullDown (getFesIdol model Visualist) Visualist ]
        ]


writeFesIdol : Model -> Html Msg
writeFesIdol model =
    tr []
        [ td [] [ text (statusHeader Idol) ]
        , td [] [ text (Idol.toString model.leader.idol) ]
        , td [] [ text (Idol.toString model.vocalist.idol) ]
        , td [] [ text (Idol.toString model.center.idol) ]
        , td [] [ text (Idol.toString model.dancer.idol) ]
        , td [] [ text (Idol.toString model.visualist.idol) ]
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
      -- | MemoryGage_plusplus
      -- | MemoryGage_plus
      -- | MemoryGage_minus
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
    | Spotlighted -- 注目の的
    | Humble -- ひかえめ
    | Perfectly
    | AppealUp_theMoreMemoryGage
    | AppealUp_theLessMemoryGage
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

        Spotlighted ->
            "注目の的"

        Humble ->
            "ひかえめ"

        Perfectly ->
            "パーフェクトリィ"

        AppealUp_theMoreMemoryGage ->
            "アピールUP（思い出高）"

        AppealUp_theLessMemoryGage ->
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
