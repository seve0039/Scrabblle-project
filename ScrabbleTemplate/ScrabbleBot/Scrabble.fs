﻿namespace Rubert

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

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        numPlayers    : uint32
        playerNumber  : uint32
        playerTurn    : uint32
        hand          : MultiSet.MultiSet<uint32>
        
    }

    let mkState b d pn np pt h= {board = b; dict = d; numPlayers = np; playerNumber = pn; playerTurn = pt; hand = h}

    let board st         = st.board
    let dict st          = st.dict
    
    let playerNumber st  = st.playerNumber
    let playerTurn st    = st.playerTurn
    let hand st          = st.hand

    let numPlayers st = st.numPlayers

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) (shouldPlay: bool )=

            
            if (State.playerTurn st = State.playerNumber st) then
                Print.printHand pieces (State.hand st)
                let input =  System.Console.ReadLine()
                let move = RegEx.parseMove input

                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (SMPlay move)

            let msg = recv cstream
//            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                  (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                forcePrint (sprintf "New pieces aquired: %s" (newPieces.ToString()))
                let remove = List.fold (fun acc elm -> MultiSet.removeSingle (fst(snd(elm))) acc) st.hand ms
                let add = List.fold (fun acc elm -> MultiSet.add (fst elm) (snd elm) acc) remove newPieces
                //let newBoardState = List.fold (fun acc (coords, (_, (char,points))) -> Map.add coord (char,points) acc ) st.boardState ms
                let st' = State.mkState st.board st.dict st.numPlayers st.playerNumber st.playerTurn st.hand 
                //let st' = State.mkState st.playerNumber added newBoardState st.numPLayers st.words st.board st.playerTurn
                aux st' false
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st' (pid % st.numPlayers + 1u = st.playerNumber)
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st' (pid % st.numPlayers + 1u = st.playerNumber)
            | RCM (CMPassed (pid)) ->
                let st' = st
                aux st' (pid % st.numPlayers + 1u = st.playerNumber)
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st false


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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber playerTurn handSet)
        