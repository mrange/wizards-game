open System

module JsonParser =
    (* Generic jsonr *)
    [<Struct>]
    type ParserState(input : string, pos : int) =

        override x.ToString () =    if x.IsEOS then "EOS"
                                    else sprintf "%d, %d, %s" pos (input.Length - pos) <| input.[pos].ToString()

        member x.IsEOS = pos >= input.Length

        member x.ParseChar (t : char->bool) : (char*ParserState) option = 
                let remaining = input.Length - pos
                if remaining < 1 then
                    None
                elif t input.[pos] then
                    Some (input.[pos], ParserState (input, pos + 1))
                else
                    None

        member x.ParseString (atLeast : int) (atMost : int) (t : char->int->bool) : (string*ParserState) option = 
                let remaining = input.Length - pos
                if atLeast > remaining then
                    None
                else
                    let length = input.Length

                    let mutable isMatch = true
                    let mutable iter = 0

                    let e = min atMost remaining

                    while isMatch && iter < e do
                        isMatch <- t (input.[iter + pos]) iter
                        iter <- iter + 1

                    if atLeast > iter then
                        None
                    elif isMatch then
                        Some (input.Substring(pos, iter), ParserState (input, pos + iter))
                    else
                        Some (input.Substring(pos, iter - 1), ParserState (input, pos + iter - 1))

    type ParserResult<'T>   = ('T*ParserState) option
    type Parser<'T>         = ParserState->ParserResult<'T>

    let satisfy (t : char->bool) : Parser<char> = fun ps -> ps.ParseChar t
    let satisfyString (atLeast : int) (atMost : int) (t : char->int->bool) : Parser<string> = fun ps -> 
        ps.ParseString atLeast atMost t

    let satisfyDigit                    = fun c -> c >= '0' && c <= '9'
    let satisfyHexDigit                 = fun c -> c >= '0' && c <= '9' || c >= 'a' && c < 'f' || c >= 'A' && c < 'F'
    let satisfyChar (ch : char)         = fun c -> c = ch

    let any : Parser<char> = satisfy <| fun c -> true
    let (|Any|_|) = any

    let eos : Parser<unit> = fun ps ->
        if ps.IsEOS then Some ((), ps)
        else None
    let (|EOS|_|) = eos

    let whitespace : Parser<char> = satisfy <| fun c -> Char.IsWhiteSpace c
    let (|Whitespace|_|) = whitespace

    let token (ch : char) : Parser<char> = satisfy <| satisfyChar ch
    let (|Token|_|) = token
            
    let hexDigit = satisfy satisfyHexDigit
    let (|HexDigit|_|) = hexDigit

    let digit = satisfy satisfyDigit
    let (|Digit|_|) = digit

    let tokens (str : string) : Parser<string> = satisfyString str.Length str.Length <| fun ch i -> str.[i] = ch
    let (|Tokens|_|) = tokens

    let whitespaces : Parser<string> = satisfyString 0 Int32.MaxValue <| fun ch i -> Char.IsWhiteSpace ch
    let (|Whitespaces|_|) = whitespaces

    let digits : Parser<string> = satisfyString 1 Int32.MaxValue <| fun ch i -> satisfyDigit ch
    let (|Digits|_|) = digits

    let between ((|Begin|_|) : Parser<_>) ((|End|_|) : Parser<_>) ((|Element|_|) : Parser<'T>) : Parser<'T> = function
        | Begin (_, Element (e, End (_,ps))) -> Some (e,ps)
        | _ -> None
    let (|Between|_|) = between
    
    let rec separatedBy ((|Element|_|) as element : Parser<'T>) ((|Separator|_|) as separator :  Parser<_>) : Parser<'T list> = function
        | Element (v, Separator (_, SeparatedBy element separator (vs,ps))) -> Some (v::vs, ps)
        | Element (v, ps) -> Some ([v], ps)
        | ps -> Some ([], ps)
    and (|SeparatedBy|_|) = separatedBy

    let rec many ((|Element|_|) as element : Parser<'T>) : Parser<'T list> = function
        | Element (v, Many element (vs,ps)) -> Some (v::vs, ps)
        | ps -> Some ([], ps)
    and (|Many|_|) = many

    (* JSON Specific *)
    type Json = 
        | Null
        | Boolean   of bool
        | Number    of float
        | String    of string
        | Object    of (string*Json) list
        | Array     of Json list

    let digitToInt64 ch = 
        match ch with
        | _ when satisfyDigit ch    -> (int64 ch) - (int64 '0')
        | _                         -> 0L

    let hexDigitToInt64 ch = 
        match ch with
        | _ when satisfyDigit ch        -> (int64 ch) - (int64 '0')
        | _ when ch >= 'a' && ch <= 'f' -> (int64 ch) - (int64 'a') + 10L
        | _ when ch >= 'A' && ch <= 'F' -> (int64 ch) - (int64 'A') + 10L
        | _                             -> 0L

    let wsToken ch : Parser<char> = function 
        | Whitespaces (_, Token ch pr) -> Some pr
        | _ -> None
    let (|WsToken|_|) = wsToken

    let rec jsonArray  : Parser<Json list> = function
        | Between (wsToken '[') (wsToken ']') (separatedBy jsonValue (wsToken ',')) pr -> Some pr
        | _ -> None
    and (|JsonArray|_|) = jsonArray

    and jsonMember : Parser<string*Json> = function
        | Whitespaces (_, JsonString (n, WsToken ':' (_, JsonValue (v, ps)))) -> Some ((n,v),ps)
        | _ -> None
    and (|JsonMember|) = jsonMember

    and jsonObject : Parser<(string*Json) list> = function
        | Between (wsToken '{') (wsToken '}') (separatedBy jsonMember (wsToken ',')) pr -> Some pr
        | _ -> None
    and (|JsonObject|_|) = jsonObject

    and jsonEscapedChar : Parser<char> = function
        | Token 'b' (_, ps) -> Some ('\b', ps)
        | Token 'f' (_, ps) -> Some ('\f', ps)
        | Token 'n' (_, ps) -> Some ('\r', ps)
        | Token 'r' (_, ps) -> Some ('\n', ps)
        | Token 't' (_, ps) -> Some ('\t', ps)
        | Token 'u' (_, HexDigit (d0, HexDigit (d1, HexDigit (d2, HexDigit (d3, ps))))) -> 
            let ch = 
                0x1000L*(hexDigitToInt64 d0)    +
                0x0100L*(hexDigitToInt64 d1)    +
                0x0010L*(hexDigitToInt64 d2)    +
                0x0001L*(hexDigitToInt64 d3)
            Some ((char ch), ps)
        | _ -> None
    and (|JsonEscapedChar|_|) = jsonEscapedChar
    
    and jsonChar : Parser<char> = function
        | Token '"' (_, _)                      -> None
        | Token '\\' (_, JsonEscapedChar pr)    -> Some pr
        | Any pr                                -> Some pr
        | _ -> None
    and (|JsonChar|_|) = jsonChar

    and listAsString (cs : char list) = System.String (cs |> List.toArray)

    and jsonString : Parser<string> = function
        | Between (token '"') (token '"') (many jsonChar) (cs, ps) -> Some (cs |> listAsString , ps)
        | _ -> None
    and (|JsonString|_|) = jsonString

    and jsonInteger : Parser<int64*int> = function 
        | Digits (ds, ps) -> 
            let mutable result = 0L
            for ch in ds do
                result <- 10L * result + (digitToInt64 ch)
            Some ((result,ds.Length),ps)
        | ps -> Some ((0L, 0),ps)
    and (|JsonInteger|_|) = jsonInteger

    and jsonExponent : Parser<float> = function
        | Tokens "e-"   (_,ps)  
        | Tokens "E-"   (_,ps)  -> Some (0.1, ps)
        | Tokens "e+"   (_,ps)  
        | Tokens "E+"   (_,ps)  
        | Token 'e'     (_,ps)  
        | Token 'E'     (_,ps)  -> Some (10., ps)
        | _ -> None
    and (|JsonExponent|_|) = jsonExponent
    
    and jsonExponentScale : Parser<float> = function 
        | JsonExponent (e, JsonInteger ((i,_),ps))  -> Some ((pown e (int i)),ps)
        | ps -> Some (1., ps)
    and (|JsonExponentScale|_|) = jsonExponentScale
    
    and jsonFraction : Parser<float> = function 
        | Token '.' (_, JsonInteger ((i,s), ps)) -> Some ((float i) * pown 0.1 s, ps)
        | ps -> Some (0., ps)
    and (|JsonFraction|_|) = jsonFraction

    and digit1To9 = satisfy <| fun ch -> ch >= '1' && ch <= '9'
    and (|Digit1To9|_|) = digit1To9

    and json1To9 : Parser<int> = function 
        | Digit1To9 (ch, ps) -> Some (int <| digitToInt64 ch, ps)
        | _ -> None
    and (|Json1To9|_|) = json1To9

    and jsonSign : Parser<float> = function 
        | Token '-' (_,ps) -> Some (-1., ps)
        | ps -> Some (1., ps)
    and (|JsonSign|_|) = jsonSign
   
    and makeNumber (sign : float) (d : int) (s : int) (i : int64) (f : float) (es : float) = 
            sign * ((float d) * (pown 10. (int s)) + (float i) + f) * es
    
    and jsonNumber : Parser<float> = function
        | JsonSign (sign, Token '0' (_, JsonFraction (f, JsonExponentScale (es, ps))))                      -> Some (makeNumber sign 0 1 0L f es, ps)
        | JsonSign (sign, Json1To9 (d, JsonInteger ((i,s), JsonFraction (f, JsonExponentScale (es, ps)))))  -> Some (makeNumber sign d s i f es, ps)
        | _ -> None
    and (|JsonNumber|_|) = jsonNumber

    and jsonPreValue  : Parser<Json> = function
        | Tokens    "null"  (_  ,ps)   -> Some (Null           , ps)
        | Tokens    "true"  (_  ,ps)   -> Some (Boolean true   , ps)
        | Tokens    "false" (_  ,ps)   -> Some (Boolean false  , ps)
        | JsonArray         (vs ,ps)   -> Some (Array vs       , ps)
        | JsonObject        (ms ,ps)   -> Some (Object ms      , ps)
        | JsonString        (s  ,ps)   -> Some (String s       , ps)
        | JsonNumber        (n  ,ps)   -> Some (Number n       , ps)
        | _ -> None
    and (|JsonPreValue|_|) = jsonPreValue

    and jsonValue  : Parser<Json> = function
        | Whitespaces (_, JsonPreValue pr) -> Some pr
        | _ -> None
    and (|JsonValue|_|) = jsonValue

    and jsonRootValue : Parser<Json> = function
        | JsonArray         (vs ,ps)   -> Some (Array vs       , ps)
        | JsonObject        (ms ,ps)   -> Some (Object ms      , ps)
        | _ -> None
    and (|JsonRootValue|_|) = jsonRootValue

    and jsonComplete : Parser<Json> = function
        | JsonRootValue (json, Whitespaces (_, EOS (_, ps))) -> Some (json, ps)
        | _ -> None
    and (|JsonComplete|_|) = jsonComplete

    let parse input = 
        match ParserState(input, 0) with
        | JsonComplete (v,_)-> Some v
        | _                 -> None


[<EntryPoint>]
let main argv = 

    let jsons = 
        [
            """[]"""
            """
{ 
    "test"  :null   ,
    "beta"  :[]     , 
    "alpha" : 
    {
        "inner":  "heart:\u2665"
    }
}"""
            """[true,100.23E-1 , null ,"Test\t", [true],{}]"""
            """[false]"""
        ]
    
    for json in jsons do
        match JsonParser.parse json with
        | None -> printfn "Parse of failed: %s" json
        | Some v -> printfn "%A" <| v

    0

    (*
    let satisfyAnyOf (anyOf: string)    = fun (c : char) -> anyOf.IndexOf(c) > -1
    let anyOf (t: string) : Parser<char> = satisfy <| satisfyAnyOf t
    let (|AnyOf|_|) = anyOf
            
    let inRange (inclusiveFrom: char, inclusiveTo: char) : Parser<char> = satisfy <| fun c -> c >= inclusiveFrom && c <= inclusiveTo 
    let (|InRange|_|) = inRange
            
    let map ((|Parser|_|) : Parser<'P>) (m : 'P -> 'M) : Parser<'M> = function
        | Parser (p, ps) -> Some ((m p, ps))
        | _ -> None
    let (|Map|_|) = map
    let (>>!) = map

    let combine ((|Left|_|) : Parser<'L>) ((|Right|_|) : Parser<'R>) : Parser<'L*'R> = function
        | Left (l, Right (r, ps)) -> Some ((l,r), ps)
        | _ -> None
    let (|Combine|_|) = combine
    let (.>>.) = combine

    let keepLeft (l : Parser<'L>) (r : Parser<'R>) : Parser<'L> = l .>>. r >>! (fun (lv, _) -> lv)
    let (|KeepLeft|_|) = keepLeft
    let (.>>) = keepLeft

    let keepRight (l : Parser<'L>) (r : Parser<'R>) : Parser<'R> = l .>>. r >>! (fun (_, rv) -> rv)
    let (|KeepRight|_|) = keepRight
    let (>>.) = keepRight

    let opt ((|Element|_|) as element : Parser<'T>) : Parser<'T option> = function
        | Element (v, ps)   -> Some (Some v, ps)
        | ps -> Some (None, ps)
    let (|Opt|_|) = opt
    *)

