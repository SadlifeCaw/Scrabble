namespace worderine
//TODO: DELETE ALL COMMENTS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
    open ScrabbleUtil
    
    type piece = uint * (char * int)
    type move = (coord * piece) list

    type state = {
        board         : Parser.board
        piecesOnBoard : Map<coord, char>
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        playerTurn    : uint32
        numPlayers    : uint32
        hand          : MultiSet.MultiSet<uint32>
        playerPoints  : Map<uint32, int32> //points can be negative, must be signed
        pieces        : Map<uint32, tile>
    }
    
    type dir =
        | Right
        | Down
    
    let nextCoord (x, y) dir =
        match dir with
        | Right -> (x + 1, y)
        | Down -> (x, y + 1)
    
    let previousCoord (x, y) dir =
        match dir with
        | Right -> (x - 1, y)
        | Down -> (x, y - 1)
    
    let turn = function
        | Right -> Down
        | Down -> Right

    let mkState b pob d pn pt np h pp p =
        {
            board = b
            piecesOnBoard = pob
            dict = d
            playerNumber = pn
            playerTurn = pt
            numPlayers = np
            hand = h
            playerPoints = pp
            pieces = p
        }
    let board st            = st.board
    let piecesOnBoard st    = st.piecesOnBoard
    let dict st             = st.dict
    let playerNumber st     = st.playerNumber
    let playerTurn st       = st.playerTurn
    let numberOfPlayers st  = st.numPlayers
    let hand st             = st.hand
    let playerPoints st     = st.playerPoints
    let pieces st     = st.pieces
    let piecesPutOnBoard ms = ms |> List.fold (fun acc (_,(id,_)) -> id :: acc) []
    let addPoints pid points st =
        let oldPoints = playerPoints st |> Map.tryFind pid
        
        // If the pid is not already added to the map, assume them to have 0 points (plus the ones to be added)
        let newPoints =
            match oldPoints with
            | Some s -> s + points
            | None -> points

        playerPoints st |> Map.add pid newPoints
        
    // ms : (coord, (id, (char, value)))
    let removePiecesFromHand ms st = ms |> List.fold (fun acc (_,(id,(_,_))) -> MultiSet.removeSingle id acc) (hand st)
    let addNewPiecesToHand list oldHand = list |> List.fold (fun acc (id,n) -> MultiSet.add id n acc) oldHand
    
    //TODO: idea: make a single UPDATE-STATE function instead of having many different methods
    let putPiecesOnBoard ms st =
        // Add every piece in the given multiset to the piecesOnBoard
        let newBoard = ms |> List.fold (fun acc (coord,(id,(l,v))) -> Map.add coord l acc) (piecesOnBoard st)
        newBoard
        
    // Change current turn to next player
    // Just pick the next ID in line. Overflow back to 1.
    let changePlayer st =
        if st.playerTurn = st.numPlayers then 1u
        else playerTurn st + 1u
    
    let chooseBest word1 word2 : move =
        if List.length word1 > List.length word2
        then word1
        else word2
    
    // Checks if tiles are free on the opposite direction
    // true -> not free
    // false -> free
    let checkNeighbours st (x,y) (dir: dir) =
        match dir with
        |Right -> if st.piecesOnBoard.ContainsKey(x, y+1) || st.piecesOnBoard.ContainsKey(x, y-1) then true else false
        |Down -> if st.piecesOnBoard.ContainsKey(x+1,y) || st.piecesOnBoard.ContainsKey(x-1, y) then true else false
    
    let canStartHere st (x,y) (dir: dir) =
        match dir with
        |Right -> if st.piecesOnBoard.ContainsKey(x-1, y) then false else true
        |Down -> if st.piecesOnBoard.ContainsKey(x, y-1) then false else true
                
    
    let rec findWordFromCoord (st: state) coord dir dict hand (current: move) (best: move) =
        match Map.tryFind coord st.piecesOnBoard with
        | Some c -> //char on board
            match Dictionary.step c dict with
            | Some (wordFinished, dict') ->
                let newBestMove = if wordFinished && not(st.piecesOnBoard.ContainsKey(nextCoord coord dir)) then chooseBest current best else best
                findWordFromCoord st (nextCoord coord dir) dir dict' hand current newBestMove
            | None -> best 
        | None -> // empty square
            if checkNeighbours st coord dir
            then best
            else
                MultiSet.fold (fun best' id num ->
                    let set = Map.find id st.pieces
                    Set.fold (fun acc (c, v) ->
                        match Dictionary.step c dict with
                        |None -> acc
                        |Some (wordFinished, dict'') ->                                
                            let newCurrent = (coord, (id, (c,v))) :: current
                            let newBestMove =
                                if wordFinished && not(st.piecesOnBoard.ContainsKey(nextCoord coord dir))
                                then chooseBest newCurrent acc
                                else acc
                            let newHand = MultiSet.removeSingle id hand
                            
                            if wordFinished then (newBestMove) else findWordFromCoord st (nextCoord coord dir) dir dict'' newHand newCurrent newBestMove
                               
                    ) best' set
                    
                ) best hand
    
    let findWord st =
        if Map.isEmpty st.piecesOnBoard
        then 
            findWordFromCoord st (0,0) Right st.dict st.hand [] []
        else
            let rightMove = Map.fold (fun acc coord _ -> if not(canStartHere st coord Right) then acc else chooseBest (findWordFromCoord st coord Right st.dict st.hand [] []) acc) [] st.piecesOnBoard
            let downMove = Map.fold (fun acc coord _ -> if not(canStartHere st coord Down) then acc else chooseBest (findWordFromCoord st coord Down st.dict st.hand [] []) acc) [] st.piecesOnBoard
            chooseBest rightMove downMove
            

module Scrabble =
    open System.Threading
    
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =

            let move = State.findWord st

            //ids of all pieces on hand
            let ids = st.hand |> MultiSet.fold (fun acc id n -> id :: acc) []
            
            if List.length move > 0
                then send cstream (SMPlay move)
                else
                    send cstream (SMChange ids)
                   
            let msg = recv cstream
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                
                let newPlayer = State.changePlayer st
                let newPoints = State.addPoints st.playerNumber points st
                let newBoard = State.putPiecesOnBoard ms st
                
                let removeHand  = State.removePiecesFromHand ms st
                let bestHand    = State.addNewPiecesToHand   newPieces removeHand
                
                let st' = State.mkState st.board newBoard st.dict newPlayer st.playerNumber st.numPlayers bestHand newPoints pieces
                
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                let newPlayer = State.changePlayer st
                let newBoard = State.putPiecesOnBoard ms st
                let newPoints = State.addPoints pid points st
                
                let st' = State.mkState st.board newBoard st.dict newPlayer st.playerNumber st.numPlayers st.hand newPoints pieces
                
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
           
                if pid = st.playerNumber
                    // If we failed. Do some shit
                    then printfn "hej"
                    //someone else failed. do nothing i guess?
                    else printfn "someone else failed"
                
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMChangeSuccess newPieces) ->
                // we get new tiles!
                
                let removeHand = ids |> List.fold (fun acc id -> MultiSet.removeSingle id acc) st.hand
                
                let bestHand = State.addNewPiecesToHand newPieces removeHand
                
                let newPlayer = State.changePlayer st
                let st' = State.mkState st.board st.piecesOnBoard st.dict newPlayer st.playerNumber st.numPlayers bestHand st.playerPoints pieces
                
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
       
        let dict = dictf false // we use trie
        let board = Parser.mkBoard boardP
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        fun () -> playGame cstream tiles (State.mkState board Map.empty dict playerNumber playerTurn numPlayers handSet Map.empty tiles)
        