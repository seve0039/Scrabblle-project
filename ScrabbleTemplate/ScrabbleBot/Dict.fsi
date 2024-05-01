module internal Dict

    type Dict = {
        Value : bool
        Children : System.Collections.Generic.Dictionary<char, Dict>
    }

    val empty : unit -> Dict
    val insert : string -> Dict -> Dict
    val lookup : string -> Dict -> bool
    val step : char -> Dict -> (bool * Dict) option
    val internal fillDict: dict: Dict -> words: string seq -> Dict