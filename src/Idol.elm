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


toFullName: Idol -> String
toFullName idol =
    case idol of
        Mano ->"櫻木真乃"
        Hiori->"風野灯織"
        Meguru->"八宮めぐる"
        Kogane->"月岡恋鐘"
        Kiriko->"幽谷霧子"
        Yuika->"三峰結華"
        Sakuya->"白瀬咲耶"
        Mamimi->"田中摩美々"
        Kaho->"小宮果穂"
        Rinze->"杜野凛世"
        Chiyoko->"園田智代子"
        Natsuha->"有栖川夏葉"
        Juri->"西城樹里"
        Chiyuki->"桑山千雪"
        Tenka->"大崎甜花"
        Amana->"大崎甘奈"
        Asahi->"芹沢あさひ"
        Fuyuko->"黛冬優子"
        Mei->"和泉愛依"
        Toru->"浅倉透"
        Madoka->"樋口円香"
        Koito->"福丸小糸"
        Hinana->"市川雛菜"


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
