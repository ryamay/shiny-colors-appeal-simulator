module Main exposing (Idol, Model, getStatus, idols, init, main, update, viewIdolPullDown)

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
    }


type alias FesIdol =
    { idol : Idol
    , vocal : Int
    , dance : Int
    , visual : Int
    , mental : Int
    , memoriesLevel : Int
    }


init : Model
init =
    { leader = FesIdol Mano 500 500 500 300 1
    , vocalist = FesIdol Hiori 500 500 500 300 1
    , center = FesIdol Meguru 500 500 500 300 1
    , dancer = FesIdol Kogane 500 500 500 300 1
    , visualist = FesIdol Kaho 500 500 500 300 1
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
