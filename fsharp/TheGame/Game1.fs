namespace TheGame

module Game1 =

    open System
    open System.Text

    (* Some helper functions *)
    let LinesToString (lines : seq<string>)     =   let sb = StringBuilder()
                                                    for line in lines do
                                                        ignore <| sb.AppendLine line
                                                    sb.ToString()
    let PrintLines (lines : seq<string>)        =   printf "%s" <| LinesToString lines
    let AsMap (ks : 'T->'TKey) xs               = xs |> Seq.map (fun x -> (ks x),x) |> Map.ofSeq
    let AsAliasSet (aliases : seq<string>)      = aliases |> Seq.map (fun x -> x.ToLowerInvariant()) |> Set.ofSeq
    let HasAlias (a : string) (s : 'V -> Set<string>) (m : Map<'K, 'V>) =   
            let a = a.ToLowerInvariant()
            m 
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.tryFind (fun v -> v |> s |> Set.contains a)

    (* Some keys that identifies objects, rooms and exits *)
    type RoomKey    = LivingRoom | Garden | Attic
    type ExitKey    = Door | Ladder
    type ObjectKey  = Whiskey | Bucket | Frog | Chain | PocketLint

    (* Some game related types *)
    type Edge =
        {
            Target      : RoomKey
            Exit        : ExitKey
            Description : string
        }
        static member New target edge description =
                        {
                            Target      = target
                            Exit        = edge
                            Description = description
                        }

    type Exit =
        {
            Key         : ExitKey
            Aliases     : Set<string>
            Description : string
        }
        static member New key aliases description =
                {
                    Key         = key
                    Aliases     = (sprintf "%A" key)::aliases |> AsAliasSet
                    Description = description
                }

    let AllExits =
        [
            Exit.New Door   [] "a sturdy door"
            Exit.New Ladder [] "a rangly ladder"
        ] |> AsMap (fun e -> e.Key)
    let GetExit key     = AllExits |> Map.find key
    let FindExit alias  = AllExits |> HasAlias alias (fun e -> e.Aliases)

    type Object =
        {
            Key         : ObjectKey
            Aliases     : Set<string>
            Description : string
        }
        static member New key aliases description =
                {
                    Key         = key
                    Aliases     = (sprintf "%A" key)::aliases |> AsAliasSet
                    Description = description
                }

    let AllObjects =
        [
            Object.New Whiskey      []          "a bottle of cheap whiskey"
            Object.New Bucket       []          "a leaky bucket"
            Object.New Frog         []          "a revolting frog"
            Object.New Chain        []          "a rusty chain"
            Object.New PocketLint   ["lint"]    "some pocket lint"
        ] |> AsMap (fun o -> o.Key)
    let GetObject key       = AllObjects |> Map.find key
    let FindObject alias    = AllObjects |> HasAlias alias (fun v -> v.Aliases)

    type Room =
        {
            Key                 : RoomKey
            Description         : string
            Edges               : Edge list
            mutable Objects     : ObjectKey list
        }
        static member New key description edges objects =
                        {
                            Key         = key
                            Description = description
                            Edges       = edges;
                            Objects     = objects
                        }

        member x.TryFindExit (e : ExitKey) = x.Edges |> List.tryFind (fun edge -> edge.Exit = e)

        member x.TryPickup (o : ObjectKey) =
                let found = x.Objects |> List.tryFind (fun obj -> obj = o)
                match found with
                |   None        ->  None
                |   Some find   ->  x.Objects <- x.Objects |> List.filter (fun obj -> obj <> o)
                                    Some find

        member x.Describe () =
            [
                yield sprintf "The %A" x.Key
                yield x.Description
                yield ""
                for e in x.Edges do yield sprintf "There is %s going %s from here." (GetExit e.Exit).Description e.Description
                for o in x.Objects do yield sprintf "You see %s on the floor." (GetObject o).Description
            ] |> LinesToString

    type Command =
        {
            Key                 : string
            RequiresArguments   : int
            Aliases             : Set<string>
            Description         : string
            Action              : string array -> unit
        }
        static member New key requiresArguments aliases description action =
                {
                    Key                 = key
                    RequiresArguments   = requiresArguments
                    Aliases             = key::aliases |> AsAliasSet
                    Description         = description
                    Action              = action
                }

    (* The game engine *)
    type Game() as this =
        let mutable location    = LivingRoom
        let mutable inventory   = [PocketLint]
        let mutable exit        = false
        let allRooms =
            [
                Room.New    LivingRoom
                            "You are in the living-room. A wizard is snoring loudly on the couch."
                            [
                                Edge.New Garden Door        "west"
                                Edge.New Attic  Ladder      "leading upstairs"
                            ]
                            [Whiskey; Bucket]
                Room.New    Garden
                            "You are in a beautiful garden. There is a well in front of you."
                            [
                                Edge.New LivingRoom Door    "east"
                            ]
                            [Chain; Frog]
                Room.New    Attic
                            "You are in the attic. There is a giant welding torch in the corner."
                            [
                                Edge.New LivingRoom Ladder  "leading downstairs"
                            ]
                            []
            ] |> AsMap (fun r -> r.Key)
        let GetRoom key = allRooms |> Map.find key

        let allCommands =
            [
                Command.New "Help"      0   ["?"; "h"]      "Prints this help"  <| fun args -> this.Help()
                Command.New "Exit"      0   []              "Exits the game"    <| fun args -> this.Exit()
                Command.New "Look"      0   ["l"]           "Looks around"      <| fun args -> this.Look()
                Command.New "Inventory" 0   ["i"; "inv"]    "List your stuff"   <| fun args -> this.Inventory()
                Command.New "Walk"      1   []              "Go somewhere"      <| fun args ->
                    match FindExit args.[0] with
                    | None      -> printfn "You try walking %s but realizes that doesn't makes sense" args.[0]
                    | Some exit -> this.Walk exit.Key
                Command.New "Pickup"    1   ["p"; "take";]  "Pickup stuff"      <| fun args ->
                    match FindObject args.[0] with
                    | None      -> printfn "You try picking up %s but realizes that doesn't makes sense" args.[0]
                    | Some o    -> this.Pickup o.Key
            ] |> AsMap (fun c -> c.Key)
        let GetCommand key      = allCommands |> Map.find key
        let FindCommand alias   = allCommands |> HasAlias alias (fun v -> v.Aliases)

        member x.CurrentRoom with get () = GetRoom location

        member x.AcceptLine (line : string) =
                let args = line.Split(' ')
                if args.Length > 0 then
                    let commandAlias = args.[0]
                    let found = FindCommand commandAlias

                    match found with
                    |   None    -> printfn "Unrecognized command: %s" commandAlias
                    |   Some c  ->
                            if args.Length <> c.RequiresArguments + 1 then
                                printfn "The %s (%s) command requires %d arguments" commandAlias c.Key c.RequiresArguments
                            else
                                c.Action args.[1..]
                    ()
                exit

        // Commands

        member x.Look () = printfn "%s" <| x.CurrentRoom.Describe()

        member x.Walk exit =
                match x.CurrentRoom.TryFindExit exit with
                |   None        ->  printfn "You are looking for %s but can't find it" (GetExit exit).Description
                |   Some edge   ->  printfn "You walk the way %s" edge.Description
                                    location <- edge.Target
                                    x.Look()

        member x.Pickup o =
                match x.CurrentRoom.TryPickup o with
                |   None        ->  printfn "You are looking for %s but can't find it" (GetObject o).Description
                |   Some find   ->  inventory <- find::inventory
                                    printfn "You pickup %s and add it to your inventory" (GetObject o).Description

        member x.Inventory () =
                [
                    yield "You are carrying:"
                    for o in inventory do yield sprintf "  %s" (GetObject o).Description
                ] |> PrintLines

        member x.Help () =
                [
                    yield "The following commands are accepted:"
                    for c in allCommands do yield sprintf "  %s" c.Key
                ] |> PrintLines

        member x.Exit () = exit <- true


    let GameRepl () =
        let mutable exit = false
        let game = Game()
        game.Look()
        while not exit do
            exit <- game.AcceptLine <| (Console.Write "~> "; Console.ReadLine())



