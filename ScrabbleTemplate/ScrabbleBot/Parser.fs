// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy (fun x -> System.Char.IsWhiteSpace(x)) <?> "whitespace"
    let pletter        = satisfy (fun x -> System.Char.IsLetter(x)) <?> "letter"
    let palphanumeric  = satisfy (fun x -> System.Char.IsLetterOrDigit(x)) <?> "alphanumeric"

    let spaces         = many (whitespaceChar) <?> "spaces"
    let spaces1        = many1 (whitespaceChar) <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let braces b = pchar '{' >*>. b .>*> pchar '}'
    let pid = (pletter <|> pchar '_') .>>. many (palphanumeric <|> pchar '_') |>> (fun (x,y) -> List.fold (fun acc element -> acc + (string element)) (string x) y)
    
    
    let unop op a = op >*>. a
    let binop op p1 p2 = p1 .>*> op .>*>. p2
    
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"

    let NParse   = pint32 |>> N <?> "Int"
    let VParse = pid |>> V <?> "Variable"
    let NegParse = pchar '-' >>. NParse |>> (fun x -> Mul (N -1, x)) <?> "Neg"
    let ParParse = parenthesise TermParse
    let PVParse = pPointValue >*>. ParParse |>> PV <?> "PV"
    
    let CTermParse, cref = createParserForwardedToRef<cExp>()
    let CharParParse = parenthesise CTermParse
    let CParse = pchar '\'' >>. anyChar .>> pchar '\'' |>> C <?> "CParser"
    let CVParse = pCharValue >*>. ParParse |>> CV
    let I2CParse = pIntToChar >*>. ParParse |>> IntToChar <?> "Int to Char"
    let ToUpperParse = pToUpper >*>. CharParParse |>> ToUpper
    let ToLowerParse = pToLower >*>. CharParParse |>> ToLower
    let C2IParse = pCharToInt >*>. CharParParse |>> CharToInt <?> "Char to Int"
    
    //Layers:
    do tref := choice [AddParse; SubParse; ProdParse]
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
    do aref := choice [C2IParse; NegParse; PVParse; VParse; NParse; ParParse;]
    do cref := choice [ToLowerParse; ToUpperParse; I2CParse; CVParse; CParse; CharParParse]
    
    let AexpParse = TermParse 

    let CexpParse = CTermParse

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
