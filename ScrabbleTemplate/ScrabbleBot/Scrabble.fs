﻿    namespace Rubert

    open ScrabbleUtil
    open ScrabbleUtil.ServerCommunication

    open System.IO
    open System
    open ScrabbleUtil.DebugPrint

    // The RegEx module is only used to parse human input. It is not used for the final product.

    module BotLogic =
        open ScrabbleUtil.Dictionary
        
        let dict = mkDict (File.ReadLines "Dictionaries/English.txt") None 

        let firstMove hand dict =  
            ""
        let optionToVal value =
            match value with 
            | Some x -> x
            | None -> failwith "Should never happen"

        let valToOption value =
            match value with 
            | x -> Some x

        let getCharFromString = fun (s : string) -> s.[6]
        let getCharsInHand (hand : MultiSet.MultiSet<uint32>) (pieces : Map<uint32,'a>) =
            let list = MultiSet.toList hand
            (List.map (fun elm -> (Map.find (elm) pieces).ToString() |> getCharFromString)) list
            //(Map.find (optionToVal list) pieces).ToString() |> getCharFromString

        let findWord (letters: string) (dictionaryPath: string) =
            let isValidWord (word: string) (letters: string) =
                let mutable remainingLetters = letters
                let mutable isValid = true
                if word.Length % 2 = 0 then // Check if word length is even
                    isValid <- false
                for char in word do
                    match remainingLetters.IndexOf(char) with
                    | -1 -> isValid <- false
                    | index ->
                        remainingLetters <- remainingLetters.Remove(index, 1)
                isValid
            
            let wordQuery = 
                File.ReadLines(dictionaryPath)
                |> Seq.tryFind (fun word -> isValidWord word letters)
            match wordQuery with
            | Some(word) -> word
            | None -> "No valid word found"

        let rec permute (str:string) =
            let rec permuteHelper (str: string) len =
                match len with
                | x when x <= 0 -> [""]
                | _ ->
                    [ for i = 0 to str.Length - 1 do
                        let prefix = str.[i..i]
                        let suffix = str.[0..i-1] + str.[i+1..]
                        for perm in permuteHelper suffix (len - 1) do
                            yield prefix + perm ]
            
            [ for len in [3; 5; 7] do
                for perm in permuteHelper str len do
                    yield perm ]


        let getAllCharacters (boardState) =
            boardState 
            |> Map.toSeq
            |> Seq.map (fun (coords, (char, s)) -> ((coords, char)))
            |> Seq.toList

        let readTile (boardState: Map<coord, (char * int)>) x y : char =
            match Map.tryFind (x, y) boardState with
            | Some (c, _) -> c
            | None -> '0'
            
        let lookForViableMove coords word boardState = 

            let rec checkTiles boardState startCoords index limit horizontal =
                let x = 
                    if horizontal then (fst startCoords) + index
                    else fst startCoords
                let y =
                    if horizontal then snd startCoords
                    else (snd startCoords) + index
                if readTile boardState x y = '0' then
                    if horizontal then
                        if readTile boardState x (y + 1) = '0' && readTile boardState x (y - 1) = '0' then
                            if index = 1 then
                                if readTile boardState (x - 1) y = '0' then
                                    Array.append [| index |] (checkTiles boardState startCoords (index + 1) limit horizontal)
                                else
                                    [| |]
                            else if index = limit then
                                if readTile boardState (x + 1) y = '0' then
                                    [| index |]
                                else
                                    [| |]
                            else 
                                Array.append [| index |] (checkTiles boardState startCoords (index + 1) limit horizontal)
                        else [| |]
                    else
                        if readTile boardState (x + 1) y = '0' && readTile boardState (x - 1) y = '0' then
                            if index = 1 then
                                if readTile boardState x (y - 1) = '0' then
                                    Array.append [| index |] (checkTiles boardState startCoords (index + 1) limit horizontal)
                                else
                                    [| |]
                            else if index = limit then
                                if readTile boardState x (y + 1) = '0' then
                                    [| index |]
                                else
                                    [| |]
                            else 
                                Array.append [| index |] (checkTiles boardState startCoords (index + 1) limit horizontal)
                        else [| |]
                else 
                    [| |]


            let horizontalPlacementArray = checkTiles boardState coords 1 (List.length word) true
            if(Array.length horizontalPlacementArray = List.length word) then
                horizontalPlacementArray
            else
                let verticalPlacementArray = checkTiles boardState coords 1 (List.length word) false
                if(Array.length verticalPlacementArray = List.length word) then
                    verticalPlacementArray
                else [| |]


            
                    

        let findWords (characters : char list) (trie : Dict) =
            let rec backtrack (chars : char list) (trieNode : Dict) (prefix : string) (validWords : string list) =
                match chars with
                | [] -> // End of characters
                    if lookup prefix trie then
                        prefix :: validWords // If the current prefix forms a valid word, add it to the list
                    else
                        validWords
                | c :: rest ->
                    match step c trieNode with
                    | Some (isWord, nextNode) ->
                        let updatedPrefix = prefix + string c
                        let nextValidWords =
                            if isWord then
                                updatedPrefix :: validWords // If we reached a valid word, add it to the list
                            else
                                validWords
                        // Recursively backtrack with remaining characters
                        backtrack rest nextNode updatedPrefix nextValidWords
                    | None -> // If no valid step found, return current valid words
                        validWords

            // Start the backtrack with an empty prefix and the root of the trie
            backtrack characters trie "" []



        let rec recursiveStep (nextStep : option<bool * Dict>) (myString : string)  (indexTracker : int) =      

            match nextStep with   
            | Some (_, dict) -> 
                if indexTracker = String.length myString then 
                    nextStep
                else 
                    let newStep = step myString[indexTracker] dict // This line ensures the computation is done
                    recursiveStep newStep myString (indexTracker+1)
            | None -> None
        let iterateOverList(wordList : list<string>) = 
            let mutable results : string list = []
            for str in wordList do 
                let mutable addToList = false
                let mutable boolTester : (bool*Dict) option = None
                
                let initialRecu= step str.[0] (dict false)
                boolTester <- recursiveStep initialRecu str 1

                match boolTester with 
                | Some(true, _)  -> 
                    results <- str :: results
                    
                | _ -> 
                    ()
            
            results
        let listToString (wordList : list<char * coord>) = 
            let mutable result = ""
            for str in wordList do 
                result <- result + string (fst str) + "\n"
            result
        let secondMove (hand) (boardState : Map<coord, (char * int)>) (pieces : Map<uint32, tile>) d = 
            let boardChars = getAllCharacters boardState
            let inHand = getCharsInHand hand pieces
            let word list = []
            let mutable word = ""
            let rec findNextPiece  acc d =
                if acc = List.length boardChars then
                    word
                else 
                match boardChars[acc] with
                | (coords, char) -> 
                    let check =lookup (word + string char) d
                    match check with
                    | true -> 
                        printfn "The word is in the dictionary %s" word
                        word
                    | false ->
                        word <- word + string char
                        let rec checkHand rest = 
                            match rest with
                            | [] -> 
                                //TODO should run find next piece with acc+1
                                printfn "No more letters in hand"
                                word
                            | char :: rest -> 
                                let check2 = (lookup (word + (string char)) d)
                                match check2 with
                                | true -> 
                                    printfn "The word is in the dictionary %s" (word + (string char))
                                    (word + (string char))
                                | false ->
                                    let newDict = step char d
                                    let newDictValue = match newDict with
                                                        | Some (_, dict) -> dict
                                                        | None -> dict false
                                    
                                    word <- word + (string char)
                                    checkHand rest
                        checkHand inHand
                        
            findNextPiece 0 (dict false)
        


                



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
                playerNumber  : uint32
                hand          : MultiSet.MultiSet<uint32>
                board         : boardProg
                boardState    : Map<coord, (char * int)>
                numPlayers    : uint32
                playerTurn    : uint32
                dict : Dictionary.Dict
            }

        
        let mkState pn h b num board pTurn d = {playerNumber = pn; hand = h; boardState = b; numPlayers = num; board = board; playerTurn = pTurn; dict = d;} 
        //let mkState b d pn np pt h= {board = b; dict = d; numPlayers = np; playerNumber = pn; playerTurn = pt; hand = h}

        let board st         = st.board
        let dict st          = st.dict

        let playerNumber st  = st.playerNumber
        let playerTurn st    = st.playerTurn
        let hand st          = st.hand

        let numPlayers st = st.numPlayers

    module Scrabble =
        open System.Threading
        let playGame cstream pieces (st : State.state) =
            let mutable counter = 0
            let rec aux (st : State.state) = 
                if (State.playerTurn st = State.playerNumber st) then
                    Thread.Sleep(3000)
                    Print.printHand pieces (st.hand)
                    Console.WriteLine ("This is second"+BotLogic.secondMove st.hand st.boardState pieces st.dict)
                    //Used to test bot finding first word
                    let letters =String.Concat(BotLogic.getCharsInHand st.hand pieces)
                    let dictionaryPath = "Dictionaries/English.txt"
                    let word = BotLogic.findWord letters dictionaryPath
                    let allPerms = BotLogic.permute letters
                    System.Console.WriteLine("The bot found these possible moves")
                    System.Console.WriteLine(BotLogic.iterateOverList allPerms)
                    
                    let input = System.Console.ReadLine()
                    
                    counter <- counter + 1
                    let move = RegEx.parseMove input



                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    send cstream (SMPlay move)

                    let mutable newTurn = (State.playerTurn st) + 1u
                    if(newTurn = (State.numPlayers st) + 1u) then
                        newTurn <- 1u

                let msg = recv cstream
    //            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

                let mutable newTurn = (State.playerTurn st) + 1u
                if(newTurn = (State.numPlayers st) + 1u) then
                    newTurn <- 1u

                match msg with
                | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                    (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                    Thread.Sleep(5000)
                    let removedPlayedLetters = List.fold (fun acc elm -> MultiSet.removeSingle (fst(snd(elm))) acc) st.hand ms
                    System.Console.WriteLine("REMOVED::::::")
                    System.Console.WriteLine(ms)
                    let addNewLetters = List.fold (fun acc elm -> MultiSet.add (fst elm) (snd elm) acc) removedPlayedLetters newPieces
                    Thread.Sleep(500)
                    System.Console.WriteLine("ADDED::::::::::.")
                    Thread.Sleep(500)
                    let newSet = MultiSet.empty
                    let newPiecesMultiSet = List.fold (fun acc elm -> MultiSet.add (fst elm) (snd elm) acc) newSet newPieces
                    Print.printHand pieces (newPiecesMultiSet)

                    let newBoardState = List.fold (fun acc elm -> Map.add(fst elm) (snd(snd elm)) acc) st.boardState ms
                    Thread.Sleep(500)
                    System.Console.WriteLine("YOUR HAND IS NOW")
                    Print.printHand pieces (addNewLetters)


                    let st' = State.mkState st.playerNumber addNewLetters newBoardState st.numPlayers st.board newTurn st.dict


                    //let st' = State.mkState st.playerNumber added newBoardState st.numPlayers st.words st.board st.playerTurn
                    aux st'
                | RCM (CMPlayed (pid, ms, points)) ->
                    (* Successful play by other player. Update your state *)
                    let newBoardState = List.fold (fun acc elm -> Map.add(fst elm) (snd(snd elm)) acc) st.boardState ms
                    let st' = State.mkState st.playerNumber st.hand newBoardState st.numPlayers st.board newTurn st.dict
                    aux st'
                | RCM (CMPlayFailed (pid, ms)) ->
                    (* Failed play. Update your state *)
                    let st' = State.mkState st.playerNumber st.hand st.boardState st.numPlayers st.board newTurn st.dict
                    aux st'
                | RCM (CMPassed (pid)) ->
                    let st' = State.mkState st.playerNumber st.hand st.boardState st.numPlayers st.board newTurn st.dict
                    aux st'

                | RCM (CMGameOver _) -> ()
                | RCM a -> failwith (sprintf "not implmented: %A" a)
                | RGPE err ->
                    printfn "Gameplay Error:\n%A" err;
                    let st' = State.mkState st.playerNumber st.hand st.boardState st.numPlayers st.board newTurn st.dict
                    aux st'

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

            fun () -> playGame cstream tiles (State.mkState playerNumber handSet Map.empty numPlayers boardP playerTurn dict)
