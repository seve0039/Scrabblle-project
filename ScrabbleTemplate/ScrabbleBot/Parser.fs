﻿// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser            // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    let pIntToChar = pstring "intToChar"
    let pPointValue = pstring "pointValue"
    let pCharToInt = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif = pstring "if" 
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces = many whitespaceChar <?> "space"
    let spaces1 = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 =p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2 
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    let parenthesise p = pchar '(' >*>. spaces >>. p .>> spaces .>*> pchar ')'

    let pid = (pletter <|> (pchar '_')) .>>. (many palphanumeric <|> many (pchar '_')) |>> (fun (x, y) ->  string x + System.String(Array.ofList y))

    
    let unop op a = op >*>. spaces >*>. a
    let binop op a b = a .>*> spaces .>*> op .>*> spaces .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul(N(-1),x)) <?> "Neg"
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"


    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"



    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "Var"
    let PVParse = pPointValue .>> spaces >>. parenthesise TermParse |>> PV <?> "PointValue"
    let ParParse = parenthesise TermParse

    
    do tref := choice [ AddParse; SubParse; ProdParse]
    do pref := choice [MulParse; DivParse; ModParse; AtomParse;]


    let AexpParse = TermParse 

    let charParse,cref = createParserForwardedToRef<cExp>()

    let CParse = between (pchar ''') (pchar ''') (palphanumeric <|> whitespaceChar) |>> C <?> "C"
    let CVParse = pCharValue .>> spaces >>. parenthesise AexpParse |>> CV <?> "CharValue"
    let ToUpperParse = pToUpper .>> spaces >>. parenthesise charParse .>> spaces|>> ToUpper <?> "ToUpper"
    let ToLowerParse = pToLower .>> spaces >>. parenthesise charParse .>> spaces|>> ToLower <?> "ToLower"
    let IntToCharParse = pIntToChar .>> spaces >>. parenthesise AexpParse |>> IntToChar <?> "IntToChar"

    do cref := choice [ CVParse; IntToCharParse; ToUpperParse; ToLowerParse; CParse]
    let CharToIntParse = unop pCharToInt (parenthesise charParse) |>> CharToInt <?> "CharToInt"
    do aref := choice [CharToIntParse; NegParse; PVParse; VParse; NParse; ParParse;]

    let CexpParse = charParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"
    

    type word   = (char * int) list
    
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun = coord -> Result<square option, Error>
    
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }
    
    type boardFun2 = coord -> Result<square option, Error>
    
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    