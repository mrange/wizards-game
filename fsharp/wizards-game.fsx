open System
open System.Text

(* Some helper functions *)
let linesToString (lines : seq<string>)     =   let sb = StringBuilder()
                                                for line in lines do
                                                    ignore <| sb.AppendLine line
                                                sb.ToString()
let printLines (lines : seq<string>)        =   printf "%s" <| linesToString lines

(* Some keys that identifies objects, rooms and exits *)
type RoomKey        = LivingRoom | Garden | Attic
type ObjectKey      = Whiskey | Bucket | Chain | Frog
type CommandKey     = Look | Walk | Pickup | Inventory
type ExitKey        = Door | Ladder
type DirectionKey   = West | Upstairs | East | Downstairs

(* F# typically promotes immutability but we need a mutable object set *)
type ObjectSet =
    {
        mutable Objects : Set<ObjectKey>
    }
    static member New os    = {Objects = os |> Set.ofList}
    member x.Has o          = x.Objects.Contains o
    member x.Add o          = x.Objects <- x.Objects.Add o
    member x.Remove o       = x.Objects <- x.Objects.Remove o

let nodes =
    [
        LivingRoom  , "You are in the living room.\nA wizard is snoring loudly on the couch\n"
        Garden      , "You are in a beautiful garden.\nThere is a well in front of you\n"
        LivingRoom  , "You are in the attic.\nThere is a giant welding torch in the corner\n"
    ] |> Map.ofList

let edges =
    [
        LivingRoom  ,   [
                            Garden      ,   (West       , Door      )
                            Attic       ,   (Upstairs   , Ladder    )
                        ] |> Map.ofList
        Garden      ,   [
                            LivingRoom  ,   (East       , Door      )
                        ] |> Map.ofList
        Attic       ,   [
                            LivingRoom  ,   (Downstairs , Ladder    )
                        ] |> Map.ofList
    ] |> Map.ofList

let objectLocations =
    [
        LivingRoom  ,   [Whiskey; Bucket]   |> ObjectSet.New
        Garden      ,   [Chain  ; Frog]     |> ObjectSet.New
        Attic       ,   []                  |> ObjectSet.New
    ] |> Map.ofList

let objectAliases =
    [
        "whiskey"   , Whiskey
        "bucket"    , Bucket
        "chain"     , Chain
        "frog"      , Frog
    ] |> List.map (fun (k,v) -> k.ToLowerInvariant(),v) |> Map.ofList

let directionAliases =
    [
        "west"      , West
        "upstairs"  , Upstairs
        "east"      , East
        "upstairs"  , Upstairs
    ] |> List.map (fun (k,v) -> k.ToLowerInvariant(),v) |> Map.ofList

let inv = [] |> ObjectSet.New

let mutable location = LivingRoom

let look () =
    [
        yield nodes.[location]
        for (dir, exit) in edges.[location] |> Seq.map (fun kv -> kv.Value) do
            yield sprintf "There is a %A going %A from here." exit dir
        for o in objectLocations.[location].Objects do
            yield sprintf "You see a %A on the floor" o
    ] |> printLines

let walk direction =
    let next =  edges.[location] |> Seq.tryFind (fun kv -> let dir, _ = kv.Value in direction = dir)
    match next with
    | None      ->  printf "You cannot go that way."
    | Some kv   ->  location <- kv.Key
                    look ()

let pickup o =
    let os = objectLocations.[location]
    if os.Has o then
        os.Remove o
        inv.Add o
        printfn "You are now carrying the %A" o
    else
        printfn "You can't get that"

let inventory () =
    [
        yield "You are carrying:"
        if inv.Objects.IsEmpty then
            yield "Nothing"
        else
            for o in inv.Objects do
            yield sprintf "A %A" o
    ] |> printLines

let commands =
    [
        Look        ,   fun s ->    look()
        Inventory   ,   fun s ->    inventory()
        Pickup      ,   fun s ->    let o = objectAliases.TryFind s
                                    match o with
                                    | None  -> printfn "You can't find the %s" s
                                    | Some o-> pickup o
        Walk        ,   fun s ->    let d = directionAliases.TryFind s
                                    match d with
                                    | None  -> printfn "You can't walk that way"
                                    | Some d-> walk d
    ] |> List.map (fun (k,v) -> (sprintf "%A" k).ToLowerInvariant(),v) |> Map.ofList

let acceptLine (line : string) =
    let line = line.ToLowerInvariant()
    if line = "quit" then true
    else
        let args = line.Split ' '
        if args.Length < 1 then false
        else
            let command = commands.TryFind args.[0]
            match command with
            | None          -> printfn "You can't do that"
            | Some action   -> action (if args.Length < 2 then "" else args.[1])
            false

let gameRepl() =
    let mutable exit = false
    look()
    while not exit do
        exit <- acceptLine <| (Console.Write "~> "; Console.ReadLine())

gameRepl()
