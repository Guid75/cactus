module Main exposing (main, update, view)

import Array exposing (Array)
import Browser
import Cards exposing (Card)
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, centerY, column, el, fill, height, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input
import Html exposing (Html, button, div, span)
import Html.Attributes exposing (readonly)
import Html.Events exposing (onClick)
import Model exposing (GameState(..), InGameCard, Model, PileChoice(..), Player, PlayerType(..))
import Msg exposing (Msg(..))
import Process
import Random
import Random.List
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


dummyPlayer =
    Player Human "dummy" [] 0


init : () -> ( Model, Cmd Msg )
init flags =
    ( { players = Array.empty
      , pile = []
      , discard = []
      , state = Waiting 4
      , messages = []
      , handSize = 4
      , currentPlayerIndex = 0
      , pickedCard = Nothing
      , cardFromTheHand = Nothing
      , cactusPlayers = []
      }
    , Cmd.none
    )


computePlayerCardsSum : Player -> Int
computePlayerCardsSum player =
    List.foldl
        (\card sum -> sum + Cards.cardValue card.card)
        0
        player.cards


collectCardsScoreForPlayer : Player -> Player
collectCardsScoreForPlayer player =
    { player | score = computePlayerCardsSum player }


collectCardsScore : Model -> Model
collectCardsScore model =
    let
        newPlayers =
            Array.map collectCardsScoreForPlayer model.players
    in
    { model | players = newPlayers }


computeSpecialAchievements : Model -> Model
computeSpecialAchievements model =
    model


computeScores : Model -> Model
computeScores model =
    model
        |> collectCardsScore
        |> computeSpecialAchievements


takeCardsFromPile : Model -> Int -> ( List Card, Model )
takeCardsFromPile model count =
    let
        extracted =
            List.take count model.pile
    in
    ( extracted, { model | pile = List.drop count model.pile } )


takeCardsFromDiscard : Model -> Int -> ( List Card, Model )
takeCardsFromDiscard model count =
    let
        extracted =
            List.take count model.discard
    in
    ( extracted, { model | discard = List.drop count model.discard } )


indexToCoordinates : Int -> Int -> ( Int, Int )
indexToCoordinates index handSize =
    let
        itemsByRow =
            handSize // 2

        y =
            index // itemsByRow

        x =
            modBy itemsByRow index
    in
    ( x, y )


distributeCards : Model -> ( List InGameCard, Model )
distributeCards model =
    let
        ( cards, newModel ) =
            takeCardsFromPile model model.handSize

        cardToInGameCard index card =
            let
                ( x, y ) =
                    indexToCoordinates index model.handSize
            in
            InGameCard x y card False

        inGameCards =
            List.indexedMap cardToInGameCard cards
    in
    ( inGameCards
    , newModel
    )


preparePlayer : Player -> ( Model, Array Player ) -> ( Model, Array Player )
preparePlayer player ( model, players ) =
    let
        ( cards, newModel ) =
            distributeCards model

        newPlayer =
            { player | cards = cards }

        newPlayers =
            Array.push newPlayer players
    in
    ( newModel, newPlayers )


preparePlayers : Model -> Model
preparePlayers model =
    let
        ( newModel, newPlayers ) =
            Array.foldl preparePlayer ( model, Array.empty ) model.players
    in
    { newModel | players = newPlayers }


addMessage : String -> Model -> Model
addMessage message model =
    { model | messages = List.append model.messages [ message ] }


tagKnownCardsPlayer : Player -> Player
tagKnownCardsPlayer player =
    let
        limit =
            List.length player.cards // 2

        newCards =
            List.indexedMap
                (\index card ->
                    if index < limit then
                        card

                    else
                        { card | known = True }
                )
                player.cards
    in
    { player | cards = newCards }


tagKnownCards : Model -> Model
tagKnownCards model =
    let
        newPlayers =
            Array.map tagKnownCardsPlayer model.players
    in
    { model | players = newPlayers }


startExamination : Model -> Model
startExamination model =
    { model | state = Examining }
        |> tagKnownCards


startGame : Model -> Model
startGame model =
    model
        |> addMessage "Players positions has been randomized"
        |> addMessage "Pile has been shuffled"
        |> preparePlayers
        |> addMessage "Cards have been distributed"
        |> addMessage "Game started!"
        |> startExamination
        |> addMessage "Click on the button when you've memorized your cards"


gameView : Model -> Html Msg
gameView model =
    Element.layout []
        (mainView model)


scoreView : Model -> Element Msg
scoreView model =
    let
        players =
            Array.toList model.players
    in
    column
        [ Border.color (rgb255 0 0 0)
        , Border.width 1
        , padding 8
        , spacing 8
        , centerY
        ]
    <|
        [ el
            [ centerX ]
          <|
            text "Scores"
        , row
            [ spacing 40 ]
            [ column
                []
              <|
                List.map (\player -> text player.name) players
            , column
                []
              <|
                List.map (\player -> el [ alignRight ] <| text <| String.fromInt player.score) players
            ]
        ]


mainView model =
    row [ width fill, height fill, centerY, spacing 10, Element.scrollbarX ]
        [ textArea model
        , cardsView model
        , scoreView model
        ]


forgeMessagesText : List String -> String
forgeMessagesText messages =
    String.join "\n" messages


textArea : Model -> Element Msg
textArea model =
    Element.Input.multiline
        [ width <| px 400
        , height <| px 600
        , Font.color (Element.rgb 0 0 0)
        , Font.size 14
        ]
        { text = forgeMessagesText model.messages
        , placeholder = Nothing
        , spellcheck = False
        , label = Element.Input.labelAbove [ Font.size 14 ] (text "Game messages")
        , onChange = OutputChanged
        }


getCactusPlayerIndex : Player -> Int -> List Player -> Maybe Int
getCactusPlayerIndex playerToFind index cactusPlayers =
    case cactusPlayers of
        head :: tail ->
            if head.name == playerToFind.name then
                Just index

            else
                getCactusPlayerIndex playerToFind (index + 1) tail

        [] ->
            Nothing


handPlayerView : Model -> Player -> Bool -> Element msg
handPlayerView model player selected =
    let
        ( backgroundColor, foregroundColor ) =
            if selected && model.state /= Examining then
                ( rgb255 0 0 0, rgb255 255 255 255 )

            else
                ( rgb255 255 255 255, rgb255 0 0 0 )

        cactusElements =
            case getCactusPlayerIndex player 0 model.cactusPlayers of
                Just index ->
                    List.map (\_ -> Element.image [] { src = "res/cactus.jpg", description = "salut" }) <| List.range 0 index

                Nothing ->
                    []
    in
    row
        [ centerX ]
        (List.concat
            [ [ el
                    [ centerX, alignBottom, Background.color backgroundColor, Font.color foregroundColor ]
                <|
                    text
                        player.name
              ]
            , cactusElements
            ]
        )


findCard : List InGameCard -> ( Int, Int ) -> Maybe InGameCard
findCard cards ( col, row ) =
    case cards of
        [] ->
            Nothing

        head :: tail ->
            if head.x == col && head.y == row then
                Just head

            else
                findCard tail ( col, row )


cardView : Model -> Player -> ( Int, Int ) -> Element Msg
cardView model player ( col, row ) =
    let
        maybeInGameCard =
            findCard player.cards ( col, row )

        ( backgroundColor, foregroundColor ) =
            case getCurrentPlayer model of
                Just currentPlayer ->
                    case model.state of
                        Examining ->
                            if row == 1 && player.playerType == Human then
                                ( rgb255 255 255 255, rgb255 0 0 200 )

                            else
                                ( rgb255 255 255 255, rgb255 200 200 200 )

                        WaitForReplaceOperation ->
                            if
                                currentPlayer.name
                                    == player.name
                                    && model.cardFromTheHand
                                    == maybeInGameCard
                                    && maybeInGameCard
                                    /= Nothing
                            then
                                ( rgb255 0 0 0, rgb255 255 255 255 )

                            else if currentPlayer.name == player.name then
                                ( rgb255 255 255 255, rgb255 0 0 0 )

                            else
                                ( rgb255 255 255 255, rgb255 200 200 200 )

                        TrashPhase ->
                            if player.playerType == Human then
                                ( rgb255 255 255 255, rgb255 0 0 0 )

                            else
                                ( rgb255 255 255 255, rgb255 200 200 200 )

                        Finished ->
                            ( rgb255 255 255 255, rgb255 0 0 0 )

                        _ ->
                            if currentPlayer.name /= player.name then
                                ( rgb255 255 255 255, rgb255 200 200 200 )

                            else
                                ( rgb255 255 255 255, rgb255 0 0 0 )

                _ ->
                    ( rgb255 255 255 255, rgb255 0 0 0 )

        str =
            case maybeInGameCard of
                Nothing ->
                    Cards.buildEmptyCard

                Just card ->
                    case model.state of
                        Examining ->
                            if row == 1 && player.playerType == Human then
                                Cards.buildDisplayCard card.card

                            else
                                Cards.buildFaceCard

                        _ ->
                            if model.state == Finished then
                                Cards.buildDisplayCard card.card

                            else
                                Cards.buildFaceCard

        eventHandler =
            case maybeInGameCard of
                Just inGameCard ->
                    [ Element.Events.onClick (CardClicked player inGameCard) ]

                Nothing ->
                    []

        pointer =
            if
                (model.state == WaitForReplaceChoice && currentPlayerIsHuman model)
                    || (model.state == TrashPhase && player.playerType == Human)
            then
                [ Element.pointer ]

            else
                []
    in
    el
        (List.concat
            [ eventHandler
            , pointer
            , [ Background.color backgroundColor
              , Font.color foregroundColor
              ]
            ]
        )
        (text str)


playerRowView : Model -> Player -> Int -> List (Element Msg)
playerRowView model player rowIndex =
    List.map (\col -> cardView model player ( col - 1, rowIndex )) <|
        List.range 1 (model.handSize // 2)


playerRowsView : Player -> Model -> List (Element Msg)
playerRowsView player model =
    [ row [] <| playerRowView model player 0
    , row [] <| playerRowView model player 1
    ]


pickedCardView : Player -> Model -> Maybe (List (Element msg))
pickedCardView player model =
    let
        txt =
            case ( player.playerType, model.pickedCard ) of
                ( Human, Just cardFromPile ) ->
                    Cards.buildDisplayCard cardFromPile

                _ ->
                    Cards.buildFaceCard

        element =
            el
                [ centerX ]
            <|
                Element.text txt
    in
    case getCurrentPlayer model of
        Just currentPlayer ->
            if currentPlayer.name == player.name && (model.state == WaitForReplaceChoice || model.state == WaitForReplaceOperation) then
                Just [ element ]

            else
                Nothing

        _ ->
            Nothing


playerIsCurrent : Player -> Model -> Bool
playerIsCurrent player model =
    case Array.get model.currentPlayerIndex model.players of
        Just currentPlayer ->
            player.name == currentPlayer.name

        _ ->
            False


currentPlayerIsHuman : Model -> Bool
currentPlayerIsHuman model =
    case getCurrentPlayer model of
        Just player ->
            player.playerType == Human

        Nothing ->
            False


handView : Model -> Player -> Element Msg
handView model player =
    column
        [ alignBottom ]
    <|
        List.concat
            [ Maybe.withDefault [] <| pickedCardView player model
            , [ handPlayerView model player <| playerIsCurrent player model ]
            , playerRowsView player model
            ]


discardView : Model -> Element Msg
discardView model =
    let
        ( backgroundColor, foregroundColor ) =
            if
                model.state
                    == WaitForPilePicked Discard
                    || (model.state == WaitForReplaceOperation && model.cardFromTheHand == Nothing)
            then
                ( rgb255 0 0 0, rgb255 255 255 255 )

            else
                ( rgb255 255 255 255, rgb255 0 0 0 )

        maybePointer =
            if model.state == WaitForPileChoice && currentPlayerIsHuman model && model.discard /= [] then
                [ Element.pointer ]

            else if model.state == WaitForReplaceChoice && currentPlayerIsHuman model then
                [ Element.pointer ]

            else
                []

        txt =
            case model.discard of
                head :: _ :: _ ->
                    Cards.buildPileCard head

                head :: _ ->
                    Cards.buildDisplayCard head

                _ ->
                    Cards.buildEmptyDiscard
    in
    el
        (List.append maybePointer
            [ Background.color backgroundColor
            , Font.color foregroundColor
            , Element.Events.onClick DiscardClicked
            ]
        )
        (text txt)


pileView : Model -> Element Msg
pileView model =
    let
        ( backgroundColor, foregroundColor ) =
            if model.state == WaitForPilePicked Pile then
                ( rgb255 0 0 0, rgb255 255 255 255 )

            else
                ( rgb255 255 255 255, rgb255 0 0 0 )

        maybePointer =
            if model.state == WaitForPileChoice && currentPlayerIsHuman model then
                [ Element.pointer ]

            else
                []
    in
    el
        (List.append maybePointer
            [ Background.color backgroundColor
            , Font.color foregroundColor
            , Element.Events.onClick PileClicked
            ]
        )
        (text Cards.buildPile)


cardsView : Model -> Element Msg
cardsView model =
    column
        [ height fill, spacing 14 ]
        [ row
            [ Border.rounded 3
            , padding 4
            , spacing 20
            , centerY
            , centerX
            , Font.family
                [ Font.typeface "Monospace"
                , Font.sansSerif
                ]
            , Font.size 14
            ]
          <|
            [ pileView model, discardView model ]
        , row
            [ Border.rounded 3
            , padding 4
            , spacing 20
            , alignBottom
            , Font.family
                [ Font.typeface "Monospace"
                , Font.sansSerif
                ]
            , Font.size 14
            ]
          <|
            Array.toList <|
                Array.map (handView model) model.players
        , controlBar model
        ]


controlBarContent model =
    case model.state of
        Examining ->
            [ Element.html <|
                Html.button [ onClick ExaminateEnd ] [ Html.text "I've seen enough, let's start!" ]
            ]

        TrashPhase ->
            [ Element.html <|
                Html.button [ onClick StopTrashPhase ] [ Html.text "End trash time" ]
            ]

        Finished ->
            [ el [ centerX ] <| text "Game over" ]

        _ ->
            case getCurrentPlayer model of
                Just player ->
                    if player.playerType == Bot then
                        [ Element.html <|
                            Html.button [ onClick RunBotStep ] [ Html.text "Next bot action" ]
                        ]

                    else if player.playerType == Human && model.state == SubmitCactusOrNot then
                        [ Element.html <|
                            Html.button [ onClick RaiseCactus ] [ Html.text "Cactus!" ]
                        , Element.html <|
                            Html.button [ onClick NextPlayer ] [ Html.text "next player" ]
                        ]

                    else
                        []

                Nothing ->
                    []


controlBar model =
    el
        [ height <| px 40 ]
    <|
        row [] <|
            controlBarContent model


startView model =
    let
        playerCount =
            case model.state of
                Waiting count ->
                    count

                _ ->
                    0

        caption =
            "How many opponents? " ++ (String.fromInt <| playerCount)
    in
    div []
        [ Html.label [ Html.Attributes.for "opponents" ] [ Html.text caption ]
        , div []
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.name "opponents"
                , Html.Attributes.min "1"
                , Html.Attributes.max "4"
                , Html.Attributes.list "opponents-count"
                , Html.Attributes.value <| String.fromInt playerCount
                , Html.Events.onInput PlayerCountChanged
                ]
                []
            ]
        , button [ onClick StartGame ] [ Html.text "Start" ]
        ]


view model =
    div
        []
        [ case model.state of
            Waiting _ ->
                startView model

            _ ->
                gameView model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


populatePlayers : Model -> Model
populatePlayers model =
    let
        playerCount =
            case model.state of
                Waiting count ->
                    count

                _ ->
                    0

        createBot index =
            Player Bot ("Bot" ++ String.fromInt index) [] 0

        bots =
            Array.map createBot <| (Array.fromList <| List.range 1 playerCount)

        myself =
            Player Human "Myself" [] 0
    in
    { model | players = Array.push myself bots }


delay : Float -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


getFirstUnknownCard : List InGameCard -> Maybe InGameCard
getFirstUnknownCard cards =
    case cards of
        head :: tail ->
            if not head.known then
                Just head

            else
                getFirstUnknownCard tail

        _ ->
            Nothing


getFirstCardLowerThan : List InGameCard -> Int -> Maybe InGameCard
getFirstCardLowerThan cards topValue =
    let
        getMaxValue inGameCard ( currentValue, currentCard ) =
            let
                inGameCardValue =
                    Cards.cardValue inGameCard.card
            in
            if inGameCardValue > currentValue && inGameCardValue > topValue then
                ( inGameCardValue, Just inGameCard )

            else
                ( currentValue, currentCard )

        ( _, card ) =
            List.foldl getMaxValue ( 0, Nothing ) cards
    in
    card


replaceInList : List InGameCard -> InGameCard -> Card -> List InGameCard
replaceInList inGameCards toReplaceCard replacingCard =
    case inGameCards of
        head :: tail ->
            if head == toReplaceCard then
                { toReplaceCard | card = replacingCard, known = True } :: tail

            else
                head :: replaceInList tail toReplaceCard replacingCard

        [] ->
            inGameCards


replaceHandCardByPickedOne : Model -> Model
replaceHandCardByPickedOne model =
    let
        newModel =
            case ( getCurrentPlayer model, model.cardFromTheHand, model.pickedCard ) of
                ( Just player, Just cardFromTheHand, Just pickedCard ) ->
                    let
                        newPlayer =
                            { player | cards = replaceInList player.cards cardFromTheHand pickedCard }
                    in
                    { model
                        | discard = cardFromTheHand.card :: model.discard
                        , players = Array.set model.currentPlayerIndex newPlayer model.players
                    }

                ( Just player, Nothing, Just pickedCard ) ->
                    { model | discard = pickedCard :: model.discard }

                _ ->
                    model
    in
    newModel


getCurrentPlayer : Model -> Maybe Player
getCurrentPlayer model =
    Array.get model.currentPlayerIndex model.players


chooseReplacement : Model -> Model
chooseReplacement model =
    let
        newModel =
            case ( Array.get model.currentPlayerIndex model.players, model.pickedCard ) of
                ( Just player, Just pickedCard ) ->
                    let
                        unknownCard =
                            getFirstUnknownCard player.cards
                    in
                    case unknownCard of
                        Just card ->
                            { model | cardFromTheHand = unknownCard }

                        Nothing ->
                            case getFirstCardLowerThan player.cards <| Cards.cardValue pickedCard of
                                Just card ->
                                    { model | cardFromTheHand = Just card }

                                Nothing ->
                                    { model | cardFromTheHand = Nothing }

                _ ->
                    model
    in
    newModel


isCactusPlayer : Player -> Model -> Bool
isCactusPlayer player model =
    getCactusPlayerIndex player 0 model.cactusPlayers /= Nothing


setNextPlayer : Model -> Model
setNextPlayer model =
    let
        nextPlayerIndex =
            modBy (Array.length model.players) (model.currentPlayerIndex + 1)

        nextPlayer =
            Maybe.withDefault dummyPlayer <| Array.get nextPlayerIndex model.players

        text =
            case nextPlayer.playerType of
                Human ->
                    "[MySelf] It's my turn"

                Bot ->
                    "[" ++ nextPlayer.name ++ "] It's my turn"

        newModel =
            if isCactusPlayer nextPlayer model then
                { model | state = Finished }

            else
                model
    in
    { newModel | currentPlayerIndex = nextPlayerIndex }
        |> addMessage text


trashSameCardsForPlayer : Player -> Model -> List Card -> ( Player, List Card )
trashSameCardsForPlayer player model discard =
    case List.head discard of
        Just discardTop ->
            let
                ( cardsToKeep, cardsToReject ) =
                    List.partition (\inGameCard -> inGameCard.card.figure /= discardTop.figure || not inGameCard.known) player.cards

                inGameCardsToReject =
                    List.map .card cardsToReject

                newDiscard =
                    List.concat [ inGameCardsToReject, discard ]
            in
            ( { player | cards = cardsToKeep }, newDiscard )

        Nothing ->
            ( player, discard )


trashBotCards : Model -> Model
trashBotCards model =
    let
        ( updatedPlayers, updatedDiscard ) =
            Array.foldl
                (\player ( newPlayers, discard ) ->
                    let
                        ( newPlayer, newDiscard ) =
                            if player.playerType == Human then
                                ( player, discard )

                            else
                                trashSameCardsForPlayer player model discard
                    in
                    ( Array.push newPlayer newPlayers, newDiscard )
                )
                ( Array.empty, model.discard )
                model.players
    in
    { model
        | players = updatedPlayers
        , discard = updatedDiscard
    }
        |> addMessage "Trash phase! Everyone can get ride of cards equals to the discard top"


pileClicked : Model -> Model
pileClicked model =
    if currentPlayerIsHuman model && model.state == WaitForPileChoice then
        pickThePile model |> setState WaitForReplaceChoice

    else
        model


choosePile : Model -> PileChoice
choosePile model =
    case model.discard of
        discardHead :: _ ->
            if Cards.cardValue discardHead < 4 then
                Discard

            else
                Pile

        [] ->
            Pile


discardClicked : Model -> Model
discardClicked model =
    case model.discard of
        head :: tail ->
            if currentPlayerIsHuman model then
                case model.state of
                    WaitForPileChoice ->
                        pickTheDiscard model |> setState WaitForReplaceChoice

                    WaitForReplaceChoice ->
                        model
                            |> replaceHandCardByPickedOne
                            |> setState SubmitCactusOrNot

                    _ ->
                        model

            else
                model

        _ ->
            model


setState : GameState -> Model -> Model
setState state model =
    { model | state = state }


pickThePile : Model -> Model
pickThePile model =
    let
        ( cards, newModel ) =
            takeCardsFromPile model 1

        card =
            Maybe.withDefault Cards.dummyCard <| List.head cards
    in
    { newModel
        | pickedCard = Just card
        , cardFromTheHand = Nothing
    }


pickTheDiscard : Model -> Model
pickTheDiscard model =
    let
        ( cards, newModel ) =
            takeCardsFromDiscard model 1

        card =
            Maybe.withDefault Cards.dummyCard <| List.head cards
    in
    { newModel
        | pickedCard = Just card
        , cardFromTheHand = Nothing
    }


getHumanPlayer : Model -> Maybe Player
getHumanPlayer model =
    let
        filtered =
            Array.filter (\player -> player.playerType == Human) model.players
    in
    Array.get 0 filtered


removeCardFromPlayer : InGameCard -> List InGameCard -> List InGameCard
removeCardFromPlayer inGameCard cards =
    List.filter (\card -> card /= inGameCard) cards


replacePlayerByName : Array Player -> Player -> String -> Array Player
replacePlayerByName players player name =
    Array.foldl
        (\newPlayer newPlayers ->
            if newPlayer.name == name then
                Array.push player newPlayers

            else
                Array.push newPlayer newPlayers
        )
        Array.empty
        players


cardClicked : Model -> Player -> InGameCard -> ( Model, Cmd Msg )
cardClicked model player inGameCard =
    if player.playerType == Human && model.state == WaitForReplaceChoice && currentPlayerIsHuman model then
        ( { model | cardFromTheHand = Just inGameCard }
            |> replaceHandCardByPickedOne
            |> setState SubmitCactusOrNot
        , delay 1 RunBotStep
        )

    else if player.playerType == Human && model.state == TrashPhase then
        let
            newDiscard =
                inGameCard.card :: model.discard

            newPlayer =
                { player | cards = removeCardFromPlayer inGameCard player.cards }

            newModel =
                { model | players = replacePlayerByName model.players newPlayer player.name }
        in
        ( { newModel | discard = newDiscard }, Cmd.none )

    else
        ( model, Cmd.none )


raiseCactus : Model -> Model
raiseCactus model =
    case Array.get model.currentPlayerIndex model.players of
        Just player ->
            { model
                | cactusPlayers = List.append model.cactusPlayers [ player ]
            }

        Nothing ->
            model


computePlayerKnownCardSum : Player -> Int
computePlayerKnownCardSum player =
    List.foldl
        (\card sum ->
            if card.known then
                sum + Cards.cardValue card.card

            else
                sum
        )
        0
        player.cards


hasUnknownCards : Player -> Bool
hasUnknownCards player =
    List.any (\card -> not card.known) player.cards


raiseCactusIfNeeded : Model -> Model
raiseCactusIfNeeded model =
    case Array.get model.currentPlayerIndex model.players of
        Just player ->
            let
                sum =
                    computePlayerKnownCardSum player
            in
            if sum <= 5 && not (hasUnknownCards player) then
                raiseCactus model

            else
                model

        Nothing ->
            model


stopTrashPhase : Model -> ( Model, Cmd Msg )
stopTrashPhase model =
    let
        newModel =
            model |> setNextPlayer |> trashBotCards
    in
    if newModel.state == Finished then
        ( newModel |> computeScores, Cmd.none )

    else if currentPlayerIsHuman newModel then
        ( newModel |> setState WaitForPileChoice, Cmd.none )

    else
        ( newModel |> setState WaitForPileChoice, delay 1 RunBotStep )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            let
                newModel =
                    populatePlayers model

                playerListGenerator =
                    Random.List.shuffle <| Array.toList newModel.players
            in
            ( newModel, Random.generate GetPlayerShuffle playerListGenerator )

        GetPlayerShuffle list ->
            let
                newModel =
                    { model | players = Array.fromList list }

                cardListGenerator =
                    Random.List.shuffle Cards.freshCardList
            in
            ( newModel, Random.generate GetCardShuffle cardListGenerator )

        GetCardShuffle list ->
            let
                newModel =
                    startGame { model | pile = list }
            in
            ( newModel, Cmd.none )

        --, delay 1000.0 Examinate )
        ExaminateEnd ->
            ( { model | state = WaitForPileChoice }, Cmd.none )

        RunBotStep ->
            let
                newModel =
                    case model.state of
                        WaitForPileChoice ->
                            let
                                pileChoice =
                                    choosePile model
                            in
                            model |> setState (WaitForPilePicked pileChoice)

                        WaitForPilePicked pileChoice ->
                            (case pileChoice of
                                Pile ->
                                    pickThePile model

                                Discard ->
                                    pickTheDiscard model

                                _ ->
                                    model
                            )
                                |> setState WaitForReplaceChoice

                        WaitForReplaceChoice ->
                            chooseReplacement model
                                |> setState WaitForReplaceOperation

                        WaitForReplaceOperation ->
                            replaceHandCardByPickedOne model
                                |> raiseCactusIfNeeded
                                |> setState TrashPhase

                        TrashPhase ->
                            trashBotCards model

                        Finished ->
                            model

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        RaiseCactus ->
            ( raiseCactus model
                |> setState TrashPhase
            , delay 1 RunBotStep
            )

        NextPlayer ->
            ( model
                |> setState TrashPhase
            , delay 1 RunBotStep
            )

        PlayerCountChanged str ->
            let
                count =
                    Maybe.withDefault 0 <| String.toInt str
            in
            ( { model | state = Waiting count }, Cmd.none )

        OutputChanged _ ->
            ( model, Cmd.none )

        CardClicked player inGameCard ->
            cardClicked model player inGameCard

        PileClicked ->
            ( pileClicked model, Cmd.none )

        DiscardClicked ->
            ( discardClicked model, Cmd.none )

        StopTrashPhase ->
            stopTrashPhase model
