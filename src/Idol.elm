module Idol exposing (..)


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


fromString : String -> Idol
fromString str =
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
