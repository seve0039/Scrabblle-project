    namespace Rubert

    open ScrabbleUtil
    open ScrabbleUtil.ServerCommunication

    open System.IO
    open System
    open ScrabbleUtil.DebugPrint

    // The RegEx module is only used to parse human input. It is not used for the final product.

    module BotLogic =
        open ScrabbleUtil.Dictionary
        
        let dict = mkDict (File.ReadLines "Dictionaries/English.txt") None 

        let getCharFromString = fun (s : string) -> s.[6]
        let getCharsInHand (hand : MultiSet.MultiSet<uint32>) (pieces : Map<uint32,'a>) =
            let list = MultiSet.toList hand
            (List.map (fun elm -> (Map.find (elm) pieces).ToString() |> getCharFromString)) list
            //(Map.find (optionToVal list) pieces).ToString() |> getCharFromString

        let getCharValues (hand : MultiSet.MultiSet<uint32>) (pieces : Map<uint32,'a>) =
            let handToList = MultiSet.toList hand
            let mutable pointList = []
            let charList = (List.map (fun elm -> (Map.find (elm) pieces).ToString() |> getCharFromString)) handToList
            //System.Console.WriteLine(charList)
            let pointSet =List.map (fun elm -> (Map.find (elm) pieces) ) handToList

            //For every set in pointSet, get the 2nd element, which is the default point a letter rewards
            for newSet in pointSet do
                let mutable count = 1
                for elem in newSet do
                    count <- count + 1
                    if count % 2 = 0 then
                        pointList <- snd elem :: pointList

            //Lists are in reverse order for some reason? So reverse reverse them to good order
            let pointList = List.rev pointList
            let listLength = List.length charList

            let mutable wordValueMap = Map.empty<string,string>
            let mutable moveList = []

            //Get every element of each list, convert them to string, combine them and add them to the new list
            for i in 0 .. listLength-1 do 
                let idString = handToList[i].ToString()
                let charString = charList[i].ToString()
                let pointString = pointList[i].ToString()      
                let moveString = idString + charString + pointString
                moveList <- moveString :: moveList
                wordValueMap <- Map.add charString moveString wordValueMap
                ()

            //Reverse again
            let moveList = List.rev moveList
            moveList
        let getCharValuesMap (hand : MultiSet.MultiSet<uint32>) (pieces : Map<uint32,'a>) =
            let handToList = MultiSet.toList hand
            let mutable pointList = []
            let charList = (List.map (fun elm -> (Map.find (elm) pieces).ToString() |> getCharFromString)) handToList
            //System.Console.WriteLine(charList)
            let pointSet =List.map (fun elm -> (Map.find (elm) pieces) ) handToList

            //For every set in pointSet, get the 2nd element, which is the default point a letter rewards
            for newSet in pointSet do
                let mutable count = 1
                for elem in newSet do
                    count <- count + 1
                    if count % 2 = 0 then
                        pointList <- snd elem :: pointList

            //Lists are in reverse order for some reason? So reverse reverse them to good order
            let pointList = List.rev pointList
            let listLength = List.length charList

            let mutable wordValueMap = Map.empty<string,string>
            let mutable moveList = []

            //Get every element of each list, convert them to string, combine them and add them to the new list
            for i in 0 .. listLength-1 do 
                let mutable moveString = ""
                let idString = handToList[i].ToString()
                let charString = charList[i].ToString()
                let pointString = pointList[i].ToString()      
                moveString <- idString + charString + pointString
                moveList <- moveString :: moveList
                wordValueMap <- Map.add charString moveString wordValueMap
                ()

            //Reverse again
            let moveList = List.rev moveList
            wordValueMap
        let rec permuteTwo (prefix: string) (chars: string list) =
            let rec permuteHelper (chars: string list) len =
                match len with
                | x when x <= 0 -> [[]]
                | _ ->
                    [ for i = 0 to List.length chars - 1 do
                        let prefix' = chars.[i]
                        let suffix = List.take i chars @ List.skip (i+1) chars
                        for perm in permuteHelper suffix (len - 1) do
                            yield prefix' :: perm ]
            [ for len in [2; 4; 6] do
                for perm in permuteHelper chars len do
                    yield prefix :: perm
                    yield perm @ [prefix]]









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
            
        let lookForViableMove coords word boardState topLeftCoords bottomRightCoords =
            let nextPos = 
                if(readTile boardState (fst coords) (snd coords) = (List.head word)) then 1
                else -1
            let rec checkTiles index limit horizontal =
                let x = 
                    if horizontal then (fst coords) + index * nextPos
                    else (fst coords)
                let y =
                    if horizontal then (snd coords)
                    else (snd coords) + index * nextPos
                if topLeftCoords <> (0, 0) && bottomRightCoords <> (0, 0) && (x > fst bottomRightCoords || x < fst topLeftCoords || y > snd bottomRightCoords || y < snd topLeftCoords) then
                    [| |]
                elif readTile boardState x y = '0' then
                    if horizontal then
                        if readTile boardState x (y + 1) = '0' && readTile boardState x (y - 1) = '0' then
                            if index = 1 then
                                if readTile boardState (x - nextPos * 2) y = '0' then
                                    Array.append [| (x, y) |] (checkTiles (index + nextPos) limit horizontal)
                                else
                                    [| |]
                            else if index = limit then
                                if readTile boardState (x + nextPos) y = '0' then
                                    [| (x, y) |]
                                else
                                    [| |]
                            else 
                                Array.append [| (x, y) |] (checkTiles (index + nextPos) limit horizontal)
                        else [| |]
                    else
                        if readTile boardState (x + 1) y = '0' && readTile boardState (x - 1) y = '0' then
                            if index = 1 then
                                if readTile boardState x (y - nextPos * 2) = '0' then
                                    Array.append [| (x, y) |] (checkTiles (index + nextPos) limit horizontal)
                                else
                                    [| |]
                            else if index = limit then
                                if readTile boardState x (y + nextPos) = '0' then
                                    [| (x, y) |]
                                else
                                    [| |]
                            else 
                                Array.append [| (x, y) |] (checkTiles (index + nextPos) limit horizontal)
                        else [| |]
                else 
                    [| |]

            let horizontalPlacementArray = checkTiles 1 (List.length word - 1) true
            if(Array.length horizontalPlacementArray = (List.length word - 1)) then
                horizontalPlacementArray
            else
                let verticalPlacementArray = checkTiles 1 (List.length word - 1) false
                if(Array.length verticalPlacementArray = (List.length word - 1)) then
                    verticalPlacementArray
                else [| |]

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
            let rec iterate (words : string list) acc =
                match words with
                | [] -> acc
                | str :: rest ->
                    let initialRecu = step str.[0] (dict false)
                    let boolTester = recursiveStep initialRecu str 1
                    let updatedAcc =
                        match boolTester with
                        | Some(true, _) -> str :: acc
                        | _ -> acc
                    iterate rest updatedAcc
            
            iterate wordList []


        let concatList (lst : list<list<string>>) : (string * string) list =
            let buildWordPieces strLst =
                let rec buildWordPieces' accWord accPieces = function
                    | [] -> (accWord, accPieces)
                    | (str:string) :: tail ->
                        match str.Length with
                        | 1 -> buildWordPieces' (accWord + str) accPieces tail
                        | 3 -> buildWordPieces' (accWord + string str.[1]) (accPieces + " " + str) tail
                        | _ -> buildWordPieces' (accWord + string str.[2]) (accPieces + " " + str) tail
                buildWordPieces' "" "" strLst
            
            let rec processLists accResults = function
                | [] -> List.rev accResults
                | strLst :: rest ->
                    let word, pieces = buildWordPieces strLst
                    processLists ((word, pieces) :: accResults) rest
                    
            
            processLists [] lst

        let lookupLst lst =
            let rec lookupWords accResults = function
                | [] -> accResults
                | t :: tail ->
                    //Console.WriteLine((fst t):string)
                    let isWord = lookup (fst t) (dict false)
                    match isWord with
                    | true -> lookupWords (t :: accResults) tail
                    | false -> lookupWords accResults tail
            
            let results = lookupWords [] lst
            match results with
            | [] -> ("", "")
            | hd :: _ -> hd


        let makeFirstMove (playableWords : string List) (wordsMapValue : Map<string,string>) =
            let resultString =
                playableWords.[0]
                |> Seq.mapi (fun i char ->
                    match Map.tryFind (string char) wordsMapValue with
                    | Some(value) -> $" 0 {i} {value}"
                    | _ -> "")
                |> String.concat ""
            resultString

            
        let secondMove (hand) (boardState : Map<coord, (char * int)>) (pieces : Map<uint32, tile>) d = 
            let boardChars = getAllCharacters boardState
            let inHand = getCharValues hand pieces
            let mutable output = []

            let rec findNextPiece  acc d =
                if acc >= List.length boardChars then
                    ""//TODO pass turn
                else 
                match boardChars[acc] with
                | (coords, char) -> 
                    let words =permuteTwo (char.ToString()) inHand |> concatList |> lookupLst
                    if words = ("","") then
                        findNextPiece (acc + 1) d
                    else
                        output <- words :: output
                        let idk = (lookForViableMove coords (fst words|> Seq.toList |> List.ofSeq) boardState (-7, -7) (7, 7))
                        //System.Console.WriteLine(idk)
                        let secondElementParts = snd words
                        let secondElementParts = secondElementParts.Split(' ', '\n')
                        let secondElementParts = secondElementParts[1..]
                        //printfn "%A" secondElementParts
                        
                        if idk.Length > 0 then
                            let strPart = Array.toList secondElementParts
                            let tuplePart = idk |> Array.map (fun (x, y) -> sprintf "%d %d" x y) |> Array.toList
                            let combined = List.zip strPart tuplePart
                                        |> List.collect (fun (s, t) -> [t; s])
                                        |> String.concat " "
                            printfn "%A" idk
                            combined
                        else
                            findNextPiece (acc + 1) d
                        //coords.ToString() + output.ToString()
            findNextPiece 0 (dict)
        


                



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
            let rec aux (st : State.state) = 
                if (((State.playerTurn st - 1u) % State.numPlayers st) + 1u = State.playerNumber st) then
                    Thread.Sleep(3000)

                    Print.printHand pieces (st.hand)
                    Thread.Sleep(1000)
                    //Used to test bot finding first word
                    let letters =String.Concat(BotLogic.getCharsInHand st.hand pieces)
                    let allPerms = BotLogic.permute letters
                    //System.Console.WriteLine("The bot found these possible moves")
                    
                    //System.Console.WriteLine(((BotLogic.permuteTwo "A" (BotLogic.getCharValues st.hand pieces))|> BotLogic.concatList)|> BotLogic.lookupLst)
                    //BotLogic.step3
                    let playableWords = BotLogic.iterateOverList allPerms
                    let mapValues = BotLogic.getCharValuesMap st.hand pieces

                    



                    if (st.boardState.IsEmpty) then
                        send cstream (SMPlay (RegEx.parseMove (BotLogic.makeFirstMove playableWords mapValues)))
                    else 
                        let secondMove = BotLogic.secondMove st.hand st.boardState pieces st.dict
                        if secondMove = ""then  
                            send cstream (SMPass)
                        else
                            send cstream (SMPlay (RegEx.parseMove (secondMove)))

                let msg = recv cstream
    //            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

                let newTurn = (State.playerTurn st) + 1u

                match msg with
                | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                    (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                    let removedPlayedLetters = List.fold (fun acc elm -> MultiSet.removeSingle (fst(snd(elm))) acc) st.hand ms
                    //System.Console.WriteLine("REMOVED::::::")
                    //System.Console.WriteLine(ms)
                    let addNewLetters = List.fold (fun acc elm -> MultiSet.add (fst elm) (snd elm) acc) removedPlayedLetters newPieces
                    //System.Console.WriteLine("ADDED::::::::::.")
                    let newSet = MultiSet.empty
                    let newPiecesMultiSet = List.fold (fun acc elm -> MultiSet.add (fst elm) (snd elm) acc) newSet newPieces
                    //Print.printHand pieces (newPiecesMultiSet)

                    let newBoardState = List.fold (fun acc elm -> Map.add(fst elm) (snd(snd elm)) acc) st.boardState ms
                    //System.Console.WriteLine("YOUR HAND IS NOW")
                    //Print.printHand pieces (addNewLetters)


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
