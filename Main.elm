module Main exposing (..)

import Html exposing (..)
import Random.List
import Random


type Color
    = Heart
    | Diamond
    | Spade
    | Clover


type alias Card =
    { value : Int
    , color : Color
    , visible : Bool
    }


type alias Deck =
    List Card


type alias Player =
    { deck : Deck
    , name : String
    , score : Int
    }


type alias GameBoard =
    { deck : Deck
    , players : List Player
    , currentPlayer : Maybe Player
    }


type Msg
    = ShuffleDeck (List Card)


initialDeck : Deck
initialDeck =
    [ Heart, Diamond, Spade, Clover ]
        |> List.map
            (\color ->
                ((List.range 1 13)
                    |> List.map (\value -> { value = value, color = color, visible = True })
                )
            )
        |> List.concat


distributeFourCardsTo : Player -> GameBoard -> GameBoard
distributeFourCardsTo currentPlayer gameBoard =
    let
        newGameBoardDeck =
            List.drop 4 gameBoard.deck

        newPlayerDeck =
            List.take 4 gameBoard.deck

        newPlayers =
            gameBoard.players
                |> List.map
                    (\player ->
                        if player.name == currentPlayer.name then
                            { player | deck = newPlayerDeck }
                        else
                            player
                    )
    in
        { gameBoard | deck = newGameBoardDeck, players = newPlayers }


distributeCards : GameBoard -> GameBoard
distributeCards gameBoard =
    List.foldr (\player -> distributeFourCardsTo player) gameBoard gameBoard.players


initialPlayer : String -> Player
initialPlayer name =
    { score = 0
    , name = name
    , deck = []
    }


initialGameBoard : GameBoard
initialGameBoard =
    { deck = initialDeck
    , players = []
    , currentPlayer = Nothing
    }


addPlayer : String -> GameBoard -> GameBoard
addPlayer playerName board =
    { board | players = (initialPlayer playerName) :: board.players }


main : Program Never GameBoard Msg
main =
    let
        gameBoard =
            initialGameBoard |> addPlayer "Thibaut" |> addPlayer "Charlon"
    in
        Html.program
            { init = ( gameBoard, Random.generate ShuffleDeck (Random.List.shuffle gameBoard.deck) )
            , view = view
            , update = update
            , subscriptions = (\e -> Sub.none)
            }


update : Msg -> GameBoard -> ( GameBoard, Cmd Msg )
update message gameBoard =
    case message of
        ShuffleDeck cards ->
            let
                newGameBoard =
                    { gameBoard | deck = cards }
            in
                ( newGameBoard |> distributeCards, Cmd.none )


colorView : Color -> String
colorView color =
    case color of
        Spade ->
            "♠"

        Heart ->
            "♥"

        Clover ->
            "♣"

        Diamond ->
            "◆"


cardView : Card -> Html Msg
cardView card =
    button []
        [ text
            (if card.visible then
                (card.value |> toString) ++ (card.color |> colorView)
             else
                "XX"
            )
        ]


playerView : Player -> Html Msg
playerView player =
    div []
        [ div [] [ text player.name ]
        , div []
            (player.deck |> List.map cardView)
        ]


view : GameBoard -> Html Msg
view gameBoard =
    div []
        [ div []
            (gameBoard.players
                |> List.map playerView
            )
        ]
