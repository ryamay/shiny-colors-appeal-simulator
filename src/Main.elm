module Main exposing (Idol, MemoryLevel, Model, getStatus, idols, main, update, viewIdolPullDown)

import Browser
import Html exposing (..)
import Html.Attributes exposing (max, min, selected, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra as Events
import Html.Extra as Html



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
    { idol : Idol
    , vocal : Int
    , dance : Int
    , visual : Int
    , mental : Int
    , memoryLevel : MemoryLevel
    }


type alias IdolAppealParam =
    { idol : Idol
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
    { leader = FesIdol Meguru 500 500 500 300 One
    , vocalist = FesIdol Hiori 500 500 500 300 One
    , center = FesIdol Mano 500 500 500 300 One
    , dancer = FesIdol Kogane 500 500 500 300 One
    , visualist = FesIdol Kaho 500 500 500 300 One
    , idolAppealParam = IdolAppealParam Mano Perfect 1.0 1.0 1.0 0.0
    , buffs = Buffs 0 0 0
    }



-- UPDATE


type Msg
    = ChangeFesIdol FesUnitPosition FesIdolStatus String
    | ChangeAppealer String
    | ChangeAppealCoefficient String
    | ChangeAppealPower AppealType String
    | ChangeMemoryAppealCoefficient String
    | ChangeBuff AppealType String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeFesIdol position status value ->
            updateFesIdol model position status value

        ChangeAppealer appealer ->
            let
                oldParam =
                    model.idolAppealParam

                newParam =
                    { oldParam | idol = toIdol appealer }
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


updateFesIdol : Model -> FesUnitPosition -> FesIdolStatus -> String -> Model
updateFesIdol model position status value =
    let
        oldIdol =
            getFesIdol model position

        newIdol =
            case status of
                Idol ->
                    { oldIdol | idol = toIdol value }

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
    div []
        [ div []
            [ h1 [] [ text "フェスアピール値シミュレータ" ]
            ]
        , div []
            [ viewJudgeArea model
            , viewBuffArea model
            , viewAppealArea model
            , viewFesIdolArea model
            ]
        ]


viewJudgeArea : Model -> Html msg
viewJudgeArea model =
    table []
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
            1.0
                + (List.map convertToUnitBuff unitIdolsMemoryLv
                    |> List.sum
                  )
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


sameIdol : Idol -> FesIdol -> Bool
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
    table []
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
    table []
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


viewAppealIdolPulldown : Model -> Html Msg
viewAppealIdolPulldown model =
    -- フェスユニットのアイドルを選択肢に表示するプルダウンを表示
    select
        [ Events.onChange ChangeAppealer ]
        --(List.map (viewMemoryLevelOption (getStatus fesIdol MemoryLevel)) (List.range 0 5))
        (List.map (viewIdolOption (toString model.idolAppealParam.idol)) (listFesUnitMember model))


listFesUnitMember : Model -> List Idol
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
        , option [ selected (memoryAppealCoefficient == 0.5), value "1.0" ] [ text "Bad" ] --FIXME: Badの場合、係数は1.0ではなく0.5
        , option [ selected (memoryAppealCoefficient == 1.5), value "1.5" ] [ text "Good" ]
        ]



{-
   viewFesIdolArea : フェスユニット編成、ステータス設定を行うエリア
-}


viewFesIdolArea : Model -> Html Msg
viewFesIdolArea model =
    div
        []
        [ table []
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
                ]
            ]
        ]


viewFesIdolStatus : Model -> FesIdolStatus -> Html Msg
viewFesIdolStatus model status =
    tr []
        [ td [] [ text (statusHeader status) ]
        , td [] [ input [ style "width" "4em", value (getStatus (getFesIdol model Leader) status), onInput (ChangeFesIdol Leader status) ] [] ]
        , td [] [ input [ style "width" "4em", value (getStatus (getFesIdol model Vocalist) status), onInput (ChangeFesIdol Vocalist status) ] [] ]
        , td [] [ input [ style "width" "4em", value (getStatus (getFesIdol model Center) status), onInput (ChangeFesIdol Center status) ] [] ]
        , td [] [ input [ style "width" "4em", value (getStatus (getFesIdol model Dancer) status), onInput (ChangeFesIdol Dancer status) ] [] ]
        , td [] [ input [ style "width" "4em", value (getStatus (getFesIdol model Visualist) status), onInput (ChangeFesIdol Visualist status) ] [] ]
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


viewMemoryLevelOption : String -> MemoryLevel -> Html Msg
viewMemoryLevelOption selectedLevel memoryLevel =
    option
        [ selected (selectedLevel == (memoryLevel |> convertToInt |> String.fromInt))
        , value (memoryLevel |> convertToInt |> String.fromInt)
        ]
        [ text (memoryLevel |> convertToInt |> String.fromInt) ]


viewMemoryLevelPullDown : FesIdol -> FesUnitPosition -> Html Msg
viewMemoryLevelPullDown fesIdol position =
    let
        memoryLevels =
            [ Zero, One, Two, Three, Four, Five ]
    in
    select
        [ Events.onChange (ChangeFesIdol position MemoryLevel) ]
        (List.map (viewMemoryLevelOption (getStatus fesIdol MemoryLevel)) memoryLevels)


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


viewIdolPullDown : FesIdol -> FesUnitPosition -> Html Msg
viewIdolPullDown fesIdol position =
    select
        [ Events.onChange (ChangeFesIdol position Idol) ]
        (List.map (viewIdolOption (getStatus fesIdol Idol)) idols)


viewIdolOption : String -> Idol -> Html Msg
viewIdolOption selectedIdol idol =
    option
        [ selected (toString idol == selectedIdol), value (toString idol) ]
        [ text (toString idol) ]


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
            toString fesIdol.idol

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


type Idol
    = Mano
    | Hiori
    | Meguru
    | Kogane
    | Kiriko
    | Yuika
    | Sakuya
    | Mamimi
    | Kaho
    | Rinze
    | Chiyoko
    | Natsuha
    | Juri
    | Chiyuki
    | Tenka
    | Amana
    | Asahi
    | Fuyuko
    | Mei
    | Toru
    | Madoka
    | Hinana
    | Koito


idols : List Idol
idols =
    [ Mano, Hiori, Meguru, Kogane, Kiriko, Yuika, Sakuya, Mamimi, Kaho, Rinze, Chiyoko, Natsuha, Juri, Chiyuki, Tenka, Amana, Asahi, Fuyuko, Mei, Toru, Madoka, Hinana, Koito ]


toString : Idol -> String
toString idol =
    case idol of
        Mano ->
            "Mano"

        Hiori ->
            "Hiori"

        Meguru ->
            "Meguru"

        Kogane ->
            "Kogane"

        Kiriko ->
            "Kiriko"

        Yuika ->
            "Yuika"

        Sakuya ->
            "Sakuya"

        Mamimi ->
            "Mamimi"

        Kaho ->
            "Kaho"

        Rinze ->
            "Rinze"

        Chiyoko ->
            "Chiyoko"

        Natsuha ->
            "Natsuha"

        Juri ->
            "Juri"

        Chiyuki ->
            "Chiyuki"

        Tenka ->
            "Tenka"

        Amana ->
            "Amana"

        Asahi ->
            "Asahi"

        Fuyuko ->
            "Fuyuko"

        Mei ->
            "Mei"

        Toru ->
            "Toru"

        Madoka ->
            "Madoka"

        Hinana ->
            "Hinana"

        Koito ->
            "Koito"


toIdol : String -> Idol
toIdol str =
    case str of
        "Mano" ->
            Mano

        "Hiori" ->
            Hiori

        "Meguru" ->
            Meguru

        "Kogane" ->
            Kogane

        "Kiriko" ->
            Kiriko

        "Yuika" ->
            Yuika

        "Sakuya" ->
            Sakuya

        "Mamimi" ->
            Mamimi

        "Kaho" ->
            Kaho

        "Rinze" ->
            Rinze

        "Chiyoko" ->
            Chiyoko

        "Natsuha" ->
            Natsuha

        "Juri" ->
            Juri

        "Chiyuki" ->
            Chiyuki

        "Tenka" ->
            Tenka

        "Amana" ->
            Amana

        "Asahi" ->
            Asahi

        "Fuyuko" ->
            Fuyuko

        "Mei" ->
            Mei

        "Toru" ->
            Toru

        "Madoka" ->
            Madoka

        "Hinana" ->
            Hinana

        "Koito" ->
            Koito

        -- マッチしなかったらManoに設定
        _ ->
            Mano


type Unit
    = IlluminationStars
    | LAntica
    | HokagoClimaxGirls
    | Alstroemeria
    | Straylight
    | Noctchill


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


whichUnit : Idol -> Unit
whichUnit idol =
    case idol of
        Mano ->
            IlluminationStars

        Hiori ->
            IlluminationStars

        Meguru ->
            IlluminationStars

        Kogane ->
            LAntica

        Kiriko ->
            LAntica

        Yuika ->
            LAntica

        Sakuya ->
            LAntica

        Mamimi ->
            LAntica

        Kaho ->
            HokagoClimaxGirls

        Rinze ->
            HokagoClimaxGirls

        Chiyoko ->
            HokagoClimaxGirls

        Natsuha ->
            HokagoClimaxGirls

        Juri ->
            HokagoClimaxGirls

        Chiyuki ->
            Alstroemeria

        Tenka ->
            Alstroemeria

        Amana ->
            Alstroemeria

        Asahi ->
            Straylight

        Fuyuko ->
            Straylight

        Mei ->
            Straylight

        Toru ->
            Noctchill

        Madoka ->
            Noctchill

        Hinana ->
            Noctchill

        Koito ->
            Noctchill
