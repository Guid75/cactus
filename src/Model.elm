module Model exposing
    ( GameState(..)
    , InGameCard
    , Model
    , PileChoice(..)
    , Player
    , PlayerType(..)
    )

import Array exposing (Array)
import Cards exposing (Card)


type alias InGameCard =
    { x : Int
    , y : Int
    , card : Card
    , known : Bool
    }


type PlayerType
    = Human
    | Bot


type alias Player =
    { playerType : PlayerType
    , name : String
    , cards : List InGameCard
    , score : Int
    }


type PileChoice
    = Pile
    | Discard
    | Undetermined


type GameState
    = Waiting Int
    | Examining
    | WaitForPileChoice
    | WaitForPilePicked PileChoice
    | WaitForReplaceChoice
    | WaitForReplaceOperation
    | SubmitCactusOrNot
    | TrashPhase
    | Finished


type alias Model =
    { players : Array Player
    , pile : List Card
    , discard : List Card
    , state : GameState
    , messages : List String
    , handSize : Int
    , currentPlayerIndex : Int
    , pickedCard : Maybe Card
    , cardFromTheHand : Maybe InGameCard
    , cactusPlayers : List Player
    }
