module Main exposing (Idol, MemoriesLevel, Model, getStatus, idols, main, update, viewIdolPullDown)

import Browser
import Html exposing (..)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra as Events
import Html.Extra as Html



-- MAIN


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
    }


type alias FesIdol =
    { idol : Idol
    , vocal : Int
    , dance : Int
    , visual : Int
    , mental : Int
    , memoriesLevel : Int
    }


type alias IdolAppealParam =
    { idol : Idol
    , appealCoefficient : AppealCoefficient
    , vocal : Float
    , dance : Float
    , visual : Float
    }


type AppealCoefficient
    = Perfect
    | Good
    | Normal
    | Bad


init : Model
init =
    { leader = FesIdol Meguru 500 500 500 300 1
    , vocalist = FesIdol Hiori 500 500 500 300 1
    , center = FesIdol Mano 500 500 500 300 1
    , dancer = FesIdol Kogane 500 500 500 300 1
    , visualist = FesIdol Kaho 500 500 500 300 1
    , idolAppealParam = IdolAppealParam Mano Perfect 1.0 1.0 1.0
    }



-- UPDATE


type Msg
    = ChangeFesIdol FesUnitPosition FesIdolStatus String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeFesIdol position status value ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewJudgeArea model
        , viewAppealArea model
        , div
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
                    , viewFesIdolStatus model Vocal
                    , viewFesIdolMemoriesLevel model
                    ]
                ]
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

                -- TODO: 思い出アピールの算出
                -- , th [] [ text "思い出アピール" ]
                ]
            ]
        , tbody []
            [ viewJudge Vo model
            , viewJudge Da model
            , viewJudge Vi model
            ]
        ]


viewJudge : AppealType -> Model -> Html msg
viewJudge judgeType model =
    tr []
        [ td [] [ text (typeHeader judgeType) ]
        , td [] [ text (String.fromInt (calcNormalAppeal model Vo judgeType)) ]
        , td [] [ text (String.fromInt (calcNormalAppeal model Da judgeType)) ]
        , td [] [ text (String.fromInt (calcNormalAppeal model Vi judgeType)) ]
        ]


calcNormalAppeal : Model -> AppealType -> AppealType -> Int
calcNormalAppeal model appealType judgeType =
    floor (basicCoefficent model appealType * appealPower model appealType)
        * (if appealType == judgeType then
            2

           else
            1
          )


appealPower : Model -> AppealType -> Float
appealPower model appealType =
    case appealType of
        Vo ->
            model.idolAppealParam.vocal

        Da ->
            model.idolAppealParam.dance

        Vi ->
            model.idolAppealParam.visual


basicCoefficent : Model -> AppealType -> Float
basicCoefficent model appealType =
    floor (fesAppealBase model appealType * (1 + Basics.toFloat (allBuffs model) / 100) * (model.idolAppealParam.appealCoefficient |> toFloat)) |> Basics.toFloat


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
            List.filter ((==) appealer) unitIdols

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


allBuffs : Model -> Basics.Int
allBuffs model =
    -- TODO: バフの集計関数を作成する。いったんゼロ固定とする。
    0


viewAppealArea : Model -> Html msg
viewAppealArea model =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "アピールするアイドル" ]
                , th [] [ text "アピール係数" ]
                , th [] [ text "Vo倍率" ]
                , th [] [ text "Da倍率" ]
                , th [] [ text "Vi倍率" ]

                -- TODO: 思い出アピールの算出
                -- , th [] [ text "思い出アピール" ]
                ]
            ]
        , tbody []
            [ viewAppealIdolPulldown model
            , viewAppealCoefficient
            , viewAppealPower Vo model
            , viewAppealPower Da model
            , viewAppealPower Vi model
            ]
        ]


viewFesIdolStatus : Model -> FesIdolStatus -> Html Msg
viewFesIdolStatus model status =
    tr []
        [ td [] [ text (statusHeader status) ]
        , td [] [ input [ value (getStatus (getFesIdol model Leader) status), onInput (ChangeFesIdol Leader status) ] [] ]
        , td [] [ input [ value (getStatus (getFesIdol model Vocalist) status), onInput (ChangeFesIdol Vocalist status) ] [] ]
        , td [] [ input [ value (getStatus (getFesIdol model Center) status), onInput (ChangeFesIdol Center status) ] [] ]
        , td [] [ input [ value (getStatus (getFesIdol model Dancer) status), onInput (ChangeFesIdol Dancer status) ] [] ]
        , td [] [ input [ value (getStatus (getFesIdol model Visualist) status), onInput (ChangeFesIdol Visualist status) ] [] ]
        ]


viewFesIdolMemoriesLevel : Model -> Html Msg
viewFesIdolMemoriesLevel model =
    tr []
        [ td [] [ text (statusHeader MemoriesLevel) ]
        , td [] [ viewMemoriesLevelPullDown (getFesIdol model Leader) Leader ]
        , td [] [ viewMemoriesLevelPullDown (getFesIdol model Vocalist) Vocalist ]
        , td [] [ viewMemoriesLevelPullDown (getFesIdol model Center) Center ]
        , td [] [ viewMemoriesLevelPullDown (getFesIdol model Dancer) Dancer ]
        , td [] [ viewMemoriesLevelPullDown (getFesIdol model Visualist) Visualist ]
        ]


viewMemoriesLevelOption : String -> Int -> Html Msg
viewMemoriesLevelOption selectedLevel memoriesLevel =
    option
        [ selected (String.fromInt memoriesLevel == selectedLevel), value (String.fromInt memoriesLevel) ]
        [ text (String.fromInt memoriesLevel) ]


viewMemoriesLevelPullDown : FesIdol -> FesUnitPosition -> Html Msg
viewMemoriesLevelPullDown fesIdol position =
    select
        [ Events.onChange (ChangeFesIdol position MemoriesLevel) ]
        (List.map (viewMemoriesLevelOption (getStatus fesIdol MemoriesLevel)) (List.range 0 5))


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

        MemoriesLevel ->
            String.fromInt fesIdol.memoriesLevel


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

        MemoriesLevel ->
            "思い出LV"



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
    | MemoriesLevel


type MemoriesLevel
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five


toInt : MemoriesLevel -> Int
toInt memoriesLevel =
    case memoriesLevel of
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
