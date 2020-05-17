module Msg exposing (Msg(..))

import Cards exposing (Card)
import Model exposing (InGameCard, Player)


type Msg
    = GetCardShuffle (List Card)
    | GetPlayerShuffle (List Player)
    | StartGame
    | ExaminateEnd
    | PlayerCountChanged String
    | OutputChanged String
    | RunBotStep
    | CardClicked Player InGameCard
    | PileClicked
    | DiscardClicked
    | RaiseCactus
    | NextPlayer
    | StopTrashPhase
