module Cards exposing
    ( Card
    , CardColor(..)
    , CardFigure(..)
    , buildDisplayCard
    , buildEmptyCard
    , buildEmptyDiscard
    , buildFaceCard
    , buildPile
    , buildPileCard
    , cardValue
    , dummyCard
    , freshCardList
    )

import String.Interpolate exposing (interpolate)


type CardColor
    = Spade
    | Heart
    | Diamond
    | Club


type CardFigure
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type alias Card =
    { figure : CardFigure, color : CardColor }


colorToCaption : CardColor -> String
colorToCaption color =
    case color of
        Spade ->
            "♠"

        Heart ->
            "♥"

        Diamond ->
            "♦"

        Club ->
            "♣"


figureToCaption : CardFigure -> String
figureToCaption figure =
    case figure of
        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"


freshCardList =
    let
        coupleToCard ( figure, color ) =
            Card figure color
    in
    List.map coupleToCard freshCardListCouples


freshCardListCouples =
    [ ( One, Spade )
    , ( Two, Spade )
    , ( Three, Spade )
    , ( Four, Spade )
    , ( Five, Spade )
    , ( Six, Spade )
    , ( Seven, Spade )
    , ( Eight, Spade )
    , ( Nine, Spade )
    , ( Ten, Spade )
    , ( Jack, Spade )
    , ( Queen, Spade )
    , ( King, Spade )
    , ( One, Heart )
    , ( Two, Heart )
    , ( Three, Heart )
    , ( Four, Heart )
    , ( Five, Heart )
    , ( Six, Heart )
    , ( Seven, Heart )
    , ( Eight, Heart )
    , ( Nine, Heart )
    , ( Ten, Heart )
    , ( Jack, Heart )
    , ( Queen, Heart )
    , ( King, Heart )
    , ( One, Diamond )
    , ( Two, Diamond )
    , ( Three, Diamond )
    , ( Four, Diamond )
    , ( Five, Diamond )
    , ( Six, Diamond )
    , ( Seven, Diamond )
    , ( Eight, Diamond )
    , ( Nine, Diamond )
    , ( Ten, Diamond )
    , ( Jack, Diamond )
    , ( Queen, Diamond )
    , ( King, Diamond )
    , ( One, Club )
    , ( Two, Club )
    , ( Three, Club )
    , ( Four, Club )
    , ( Five, Club )
    , ( Six, Club )
    , ( Seven, Club )
    , ( Eight, Club )
    , ( Nine, Club )
    , ( Ten, Club )
    , ( Jack, Club )
    , ( Queen, Club )
    , ( King, Club )
    ]


cardValue : Card -> Int
cardValue card =
    case card.figure of
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

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            10

        Queen ->
            10

        King ->
            0


dummyCard =
    Card One Spade


buildDisplayCard : Card -> String
buildDisplayCard { figure, color } =
    let
        figureCaption =
            figureToCaption figure

        colorCaption =
            colorToCaption color

        regularPattern =
            "┌───────┐\n│{0}      │\n│       │\n│   {1}   │\n│       │\n│      {2}│\n└───────┘"

        tenPattern =
            "┌───────┐\n│{0}     │\n│       │\n│   {1}   │\n│       │\n│     {2}│\n└───────┘"
    in
    interpolate
        (case figure of
            Ten ->
                tenPattern

            _ ->
                regularPattern
        )
        [ figureCaption, colorCaption, figureCaption ]


buildPileCard : Card -> String
buildPileCard { figure, color } =
    let
        figureCaption =
            figureToCaption figure

        colorCaption =
            colorToCaption color

        regularPattern =
            "┌───────╖╖\n│{0}      ║║\n│       ║║\n│   {1}   ║║\n│       ║║\n│      {0}║║\n└───────╜╜"

        tenPattern =
            "┌───────╖╖\n│{0}     ║║\n│       ║║\n│   {1}   ║║\n│       ║║\n│     {0}║║\n└───────╜╜"
    in
    interpolate
        (case figure of
            Ten ->
                tenPattern

            _ ->
                regularPattern
        )
        [ figureCaption, colorCaption ]


buildFaceCard : String
buildFaceCard =
    "┌───────┐\n│?      │\n│       │\n│   ?   │\n│       │\n│      ?│\n└───────┘"


buildPile : String
buildPile =
    "┌───────╖╖\n│?      ║║\n│       ║║\n│   ?   ║║\n│       ║║\n│      ?║║\n└───────╜╜"


buildEmptyCard : String
buildEmptyCard =
    "         \n         \n         \n         \n         \n         \n         "


buildEmptyDiscard : String
buildEmptyDiscard =
    "┌───────┐\n│       │\n│       │\n│       │\n│       │\n│       │\n└───────┘"
