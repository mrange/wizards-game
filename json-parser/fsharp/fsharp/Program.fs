open System.Linq

module JsonParser =

    (* Generic parser *)
    [<Struct>]
    type ParserState(input : string, pos : int) =
        member x.Input          = input
        member x.Pos            = pos
        member x.IsEOS          = not (pos < input.Length)
        member x.Remaining      = input.Length - pos
        member x.Current        = input.[pos]
        member x.Advance steps  = ParserState(input, pos + steps)

    type ParserResult<'T>   = ('T*ParserState) option
    type Parser<'T>         = ParserState->ParserResult<'T>

    let satisfyChar (t : char->bool) : Parser<char> = fun ps ->
        if ps.Remaining < 1 then
            None
        elif t ps.Current then
            Some <| (ps.Current, ps.Advance 1)
        else
            None
    let (|SatisfyChar|_|) = satisfyChar

    let consumeChar (ch : char) = satisfyChar <| fun c -> c = ch
    let (|ConsumeChar|_|) = consumeChar
            
    let expectsAnyOf (anyOf: string) : Parser<char> = satisfyChar <| fun c -> anyOf.Contains(c)
    let (|ExpectsAnyOf|_|) = expectsAnyOf
            
    let expectsInRange (inclusiveFrom: char, inclusiveTo: char) : Parser<char> = satisfyChar <| fun c -> c >= inclusiveFrom && c <= inclusiveTo 
    let (|ExpectsInRange|_|) = expectsInRange
            
    let consumeString (str : string) : Parser<string>  = fun ps ->
        if ps.Remaining < str.Length then
            None
        else
            let input = ps.Input
            let pos = ps.Pos

            let mutable isMatch = true

            for i in 0..str.Length - 1 do
                isMatch <- isMatch && input.[i + pos] = str.[i]

            if isMatch then
               Some <| (str, ps.Advance str.Length)
            else
                None 
    let (|ConsumeString|_|) = consumeString
    
    let rec separatedBy ((|Element|_|) as element : Parser<'T>) ((|Separator|_|) as separator :  Parser<_>) : Parser<'T list> = function
        | Element (v, Separator (_, SeparatedBy element separator (vs,ps))) -> Some (v::vs, ps)
        | Element (v, nps) -> Some ([v], nps)
        | _ -> None
    and (|SeparatedBy|_|) = separatedBy

    let opt ((|Element|_|) as element : Parser<'T>) : Parser<'T option> = function
        | Element (v, ps)   -> Some (Some v, ps)
        | ps -> Some (None, ps)
    let (|Opt|_|) = opt

    (* JSON Specific *)
    type JsonObject = 
        | Null
        | Boolean   of bool
        | Number    of float
        | String    of string
        | Object    of (string*JsonObject) list
        | Array     of JsonObject list

    let rec parseArray  : Parser<JsonObject list> = function
        | ConsumeChar '[' (_, SeparatedBy parseValue (consumeChar ',') (jos, ConsumeChar ']' (_, ps)))  -> Some (jos,ps)
        | _ -> None
    and (|ParseArray|_|) = parseArray

    and parseMember : Parser<string*JsonObject> = function
        | ParseString (n, ConsumeChar ':' (_, ParseValue (v, ps))) -> Some ((n,v),ps)
        | _ -> None
    and (|ParseMember|) = parseMember

    and parseObject : Parser<(string*JsonObject) list> = function
        | ConsumeChar '{' (_, SeparatedBy parseMember (consumeChar ',') (jos, ConsumeChar '}' (_, ps)))  -> Some (jos,ps)
        | _ -> None
    and (|ParseObject|_|) = parseObject

    and parseString : Parser<string> = function
        | _ -> None
    and (|ParseString|_|) = parseString

    and parseInteger : Parser<int64*int> = function 
        | _ -> None 
    and (|ParseInteger|_|) = parseInteger
    
    and parseExponentScale : Parser<float> = function 
        | ps -> Some (0., ps)
    and (|ParseExponentScale|_|) = parseExponentScale
    
    and parseFraction : Parser<float> = function 
        | ParseInteger ((i,s), ps) -> Some ((float i) * pown 0.1 s, ps)
        | ps -> Some (0., ps)
    and (|ParseFraction|_|) = parseFraction

    and consume1To9 = satisfyChar <| fun ch -> ch >= '1' && ch <= '9'
    and (|Consume1To9|_|) = consume1To9

    and parse1To9 : Parser<int> = function 
        | Consume1To9 (ch, ps) -> Some ((int ch) - (int '0'), ps)
        | ps -> Some (0, ps)
    and (|Parse1To9|_|) = parse1To9

    and parseSign : Parser<float> = function 
        | ConsumeChar '-' (_,ps) -> Some (-1., ps)
        | ps -> Some (1., ps)
    and (|ParseSign|_|) = parseSign
   
    and makeNumber (sign : float) (d : int) (s : int) (i : int64) (f : float) (es : float) = 
            sign * ((float d) * (pown 10. (int s)) + (float i) + f) * es
    
    and parseNumber : Parser<float> = function
        | ParseSign (sign, ConsumeChar '0' (_, ParseFraction (f, ParseExponentScale (es, ps))))                  -> Some (makeNumber sign 0 1 0L f es, ps)
        | ParseSign (sign, Parse1To9 (d, ParseInteger ((i,s), ParseFraction (f, ParseExponentScale (es, ps)))))  -> Some (makeNumber sign d s i f es, ps)
        | _ -> None
    and (|ParseNumber|_|) = parseNumber

    and parseValue  : Parser<JsonObject> = function
        | ConsumeString "null"  (_  ,ps)   -> Some (Null           , ps)
        | ConsumeString "true"  (_  ,ps)   -> Some (Boolean true   , ps)
        | ConsumeString "false" (_  ,ps)   -> Some (Boolean false  , ps)
        | ParseArray            (vs ,ps)   -> Some (Array vs       , ps)
        | ParseObject           (ms ,ps)   -> Some (Object ms      , ps)
        | ParseString           (s  ,ps)   -> Some (String s       , ps)
        | ParseNumber           (n  ,ps)   -> Some (Number n       , ps)
        | _ -> None
    and (|ParseValue|_|) = parseValue

    let parseJson input = 
        match ParserState(input, 0) with
        | ParseValue (_,v)  -> Some v
        | _                 -> None


[<EntryPoint>]
let main argv = 
    
    let x = JsonParser.parseJson "true"

    printfn "%A" x

    0
