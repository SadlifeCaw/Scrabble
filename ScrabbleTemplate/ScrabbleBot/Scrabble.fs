namespace worderine

open System
open Eval
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.
module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

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
        points        : int32 //points can be negative, must be signed
    }
    
    //TODO RemovePiecesFromHand

    let mkState b pob d pn pt np h p = {board = b
                                        piecesOnBoard = pob
                                        dict = d
                                        playerNumber = pn
                                        playerTurn = pt
                                        numPlayers = np
                                        hand = h
                                        points = p
                                        }
    let board st         = st.board
    let piecesOnBoard st = st.piecesOnBoard
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let playerTurn st  = st.playerTurn
    let numberOfPlayers st  = st.numPlayers
    let hand st          = st.hand
    let points st = st.points
    
    // List.fold (fun acc ((x-coord,y-coord),(pieceid,(char,value))) -> id :: acc) []
    let piecesPutOnBoard ms = ms |> List.fold (fun acc (_,(id,_)) -> id :: acc) []
    let addPlayerPoints (st : state) (newPoints : int32) = points st + newPoints
    let removePiecesFromHand ms st =
        let newHand = ms |> List.fold (fun acc (_,(id,(_,_))) -> MultiSet.removeSingle id acc) (hand st)
        newHand
    let addNewPiecesToHand list oldHand =
        let newHand = list |> List.fold (fun acc (id,n) -> MultiSet.add id n acc) oldHand
        newHand
    
    //TODO: idea: make a single UPDATE-STATE function instead of having many different methods
    let putPiecesOnBoard (st : state) ms =
        // Add every piece in the given multiset to the piecesOnBoard
        let newBoard = ms |> List.fold (fun acc (coord,(id,(l,v))) -> Map.add coord (id, (l,v)) acc) (piecesOnBoard st)
        newBoard
        
    let changePlayer st =
        if st.playerTurn = st.numPlayers then 0u
        else playerTurn st + 1u
        
    // Change current turn to next player
    // Just pick the next ID in line. Overflow back to 0.

module Scrabble =
    open System.Threading
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            //debug messages for seeing if state is properly updated
            
            //printfn "Number of players: %u" st.numPlayers
            //printfn "Current player ID: %u" st.playerTurn
            //printfn "Your player ID: %u" st.playerNumber  
          
            if st.playerTurn = st.playerNumber
                then printfn "It is your turn"
                else printfn "It is not your turn"
            
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                    
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input
            // TODO 4. Make a new file - a bot which autonates moves. Give it pieces, st.piecesOnBoard, st.hand and returns a move
            
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                // TODO 1. extract id (unt32) from ms, and remove it from the hand
                // TODO 2. Add new pieces (id of the tile, amount of tiles id has been drawn) to the current hand by adding the ids to the hand
                // TODO 3. Add all coordinates and pieces from ms to st.piecesOnBoard
                // VI har lige lagt et ord på boardet, det betyder:
                // fjern tiles vi lige har brugt fra vores hånd
                // tilføj nye tiles til vores hånd
                // ændr current player
                
                let newPlayer = State.changePlayer st
                let newPoints = State.addPlayerPoints st points
                
                let removeHand  = State.removePiecesFromHand ms st
                let bestHand    = State.addNewPiecesToHand   newPieces removeHand
                
                let st' = State.mkState st.board st.piecesOnBoard st.dict newPlayer st.playerNumber st.numPlayers bestHand newPoints
                
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                // En anden spiller har lagt et ord ned. Det betyder at vi nu skal opdatere:
                // hvilke ord der er på boardet
                // ændr current player
                // ændr personens der lige har spillets point
                
                let newPlayer = State.changePlayer st
                let newBoard = State.putPiecesOnBoard st ms
                
                let st' = State.mkState st.board newBoard st.dict newPlayer st.playerNumber st.numPlayers st.hand st.points
                
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                
                // Vi har prøvet at lægge et ulovligt ord. Det betyder at vi nu skal opdatere;
                // ved faktisk ikke lige hvad reglerne er
                // betyder det at vi må prøve igen, eller mister man bare sin tur??
                
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board Map.empty dict playerNumber playerTurn numPlayers handSet 0)
        