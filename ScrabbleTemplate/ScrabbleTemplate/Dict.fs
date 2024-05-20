module Dict

open System.Collections.Generic

type Dict = {
    Value : bool
    Children : Dictionary<char, Dict>
}

let empty () =
    { Value = false; Children = new Dictionary<char, Dict>() }

let rec insert (s : string) (trie : Dict) =
    let rec insertHelper (s : string) (trieNode : Dict) (index : int) =
        if index = s.Length then
            { trieNode with Value = true }
        else
            let c = s.[index]
            let child = 
                match trieNode.Children.TryGetValue(c) with
                | (true, childNode) -> childNode
                | _ -> empty()
            let newChild = insertHelper s child (index + 1)
            trieNode.Children.[c] <- newChild
            trieNode

    insertHelper s trie 0

let rec insertMany (lines : string seq) (d: Dict) =
    let rec insertAll (lines: string seq) (trie: Dict) =
        match Seq.tryHead lines with
        | None -> trie // Base case: No more lines to insert, return the trie
        | Some l ->
            let updatedTrie = insert l trie // Insert the current line into the trie
            insertAll (Seq.skip 1 lines) updatedTrie // Recursively insert remaining lines

    let trieAcc = insertAll lines d // Recursively insert all lines into a new trie
    printf "Done!"
    trieAcc // Return the final trie

let rec lookup (s : string) (trie : Dict) =
    let rec lookupHelper (s : string) (trieNode : Dict) (index : int) =
        if index = s.Length then
            trieNode.Value
        else
            match trieNode.Children.TryGetValue(s.[index]) with
            | (true, childNode) -> lookupHelper s childNode (index + 1)
            | _ -> false

    lookupHelper s trie 0

let step (c : char) (trie : Dict) =
    match trie.Children.TryGetValue(c) with
    | (true, childNode) -> Some (childNode.Value, childNode)
    | _ -> None

//fills the dictionary with the words in the sequence and returns the dictionary
let fillDict (dict : Dict) (words : string seq) =
    let filledDict = Seq.fold (fun acc word -> insert word acc) dict words
    filledDict
