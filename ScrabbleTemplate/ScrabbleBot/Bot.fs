namespace worderine

open System
open System.Reflection

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    type coord = int * int
    type tile = char * int
    type piece = uint * tile
    type move = (coord * piece) list

    type state = {
        board         : Parser.board
        piecesOnBoard : Map<coord, piece>
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        playerTurn    : uint32
        numPlayers    : uint32
        hand          : MultiSet.MultiSet<uint32>
    }
module Bot =
    let findBestWord (st: State.state) =
        
        aux 