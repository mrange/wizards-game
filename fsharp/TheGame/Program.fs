open System
open System.Text

(* Some keys that identifies objects, rooms and exits *)

type RoomKey =
    |   LivingRoom  
    |   Garden
    |   Attic

type ExitKey =
    |   Door
    |   Ladder

type ObjectKey =
    |   Whiskey
    |   Bucket 
    |   Frog
    |   Chain
    |   PocketLint

(* Some types *)

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
        Aliases     : string list
        Description : string
    }
    static member New key aliases description = 
            {
                Key         = key
                Aliases     = (sprintf "%A" key).ToLowerInvariant()::aliases
                Description = description
            }

let MatchAlias (alias : string) (aliases : string list) =   
        aliases 
        |>  List.exists (fun a -> String.Equals(alias, a, StringComparison.InvariantCultureIgnoreCase))


let AllExits =
    [
        Exit.New Door   [] "a sturdy door"
        Exit.New Ladder [] "a rangly ladder"
    ]

let GetExit (key : ExitKey) = AllExits |> List.find (fun e -> e.Key = key)
let FindExit (alias : string) = AllExits |> List.tryFind (fun e -> MatchAlias alias e.Aliases)

type Object = 
    {
        Key         : ObjectKey
        Aliases     : string list
        Description : string
    }
    static member New key aliases description = 
            {
                Key         = key
                Aliases     = (sprintf "%A" key).ToLowerInvariant()::aliases
                Description = description
            }

let AllObjects =
    [
        Object.New Whiskey      []          "a bottle of cheap whiskey"
        Object.New Bucket       []          "a leaky bucket"
        Object.New Frog         []          "a revolting frog"
        Object.New Chain        []          "a rusty chain"
        Object.New PocketLint   ["lint"]    "some pocket lint"
    ]

let GetObject (key : ObjectKey)     = AllObjects |> List.find (fun o -> o.Key = key)
let FindObject (alias : string)  = AllObjects |> List.tryFind (fun o -> MatchAlias alias o.Aliases)

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
        let describe_edge (sb : StringBuilder) (x : Edge) =   
            ignore <| sb.AppendLine (sprintf 
                        "There is %s going %s from here." 
                        (GetExit x.Exit).Description
                        x.Description
                        )
        let describe_object (sb : StringBuilder) (x : ObjectKey) =   
            ignore <| sb.AppendLine (sprintf "You see %s on the floor." (GetObject x).Description)
        let sb = StringBuilder()
        ignore <| sb.AppendLine (sprintf "The %A" x.Key)
        ignore <| sb.AppendLine x.Description
        ignore <| sb.AppendLine()
        for edge in x.Edges do
            describe_edge sb edge
        for o in x.Objects do
            describe_object sb o
        sb.ToString()

type Command = 
    {
        Key                 : string
        RequiresArguments   : int
        Aliases             : string list
        Description         : string
        Action              : string array -> unit
    }
    static member New key requiresArguments aliases description action = 
            {
                Key                 = key
                RequiresArguments   = requiresArguments
                Aliases             = key.ToLowerInvariant()::aliases
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
            Room.New 
                LivingRoom 
                "You are in the living-room. A wizard is snoring loudly on the couch."  
                [
                    Edge.New Garden Door        "west"
                    Edge.New Attic  Ladder      "leading upstairs"
                ]
                [
                    Whiskey
                    Bucket
                ]
            Room.New 
                Garden 
                "You are in a beautiful garden. There is a well in front of you."  
                [
                    Edge.New LivingRoom Door    "east"
                ]
                [
                    Chain
                    Frog
                ]
            Room.New 
                Attic 
                "You are in the attic. There is a giant welding torch in the corner."  
                [
                    Edge.New LivingRoom Ladder  "leading downstairs"
                ]
                [
                ]
        ]

    let commands =
        [
            Command.New "Help"      0   ["?"; "h"]      "Prints this help"  <| fun args -> this.Help()
            Command.New "Exit"      0   []              "Exits the game"    <| fun args -> this.Exit()
            Command.New "Look"      0   ["l"]           "Looks around"      <| fun args -> this.Look()
            Command.New "Inventory" 0   ["i"; "inv"]    "List your stuff"   <| fun args -> this.Inventory()
            Command.New "Walk"      1   []              "Go somewhere"      <| fun args -> 
                match FindExit args.[0] with
                | None      -> printfn "You try walking %s but realizes that doesn't makes sense" args.[0]
                | Some exit -> this.Walk exit.Key
            Command.New "Pickup"    1   ["p"]           "List your stuff"   <| fun args -> 
                match FindObject args.[0] with
                | None      -> printfn "You try picking up %s but realizes that doesn't makes sense" args.[0]
                | Some o    -> this.Pickup o.Key
        ] 
    
    member x.CurrentRoom with get () = allRooms |> List.find (fun node -> node.Key = location)

    member x.AcceptLine (line : string) =
            let args = line.Split(' ')
            if args.Length > 0 then
                let commandAlias = args.[0]
                let found = commands |> List.tryFind (fun c -> MatchAlias commandAlias c.Aliases)
                
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

    member x.Walk (e : ExitKey) = 
            let current = x.CurrentRoom
            match current.TryFindExit e with
            |   None        ->  printfn "You are looking for %s but can't find it" (GetExit e).Description
            |   Some edge   ->  printfn "You walk the way %s" edge.Description
                                location <- edge.Target
                                x.Look()

    member x.Pickup (o : ObjectKey) = 
            let current = x.CurrentRoom
            match current.TryPickup o with
            |   None        ->  printfn "You are looking for %s but can't find it" (GetObject o).Description
            |   Some find   ->  inventory <- find::inventory
                                printfn "You pickup %s and add it to your inventory" (GetObject o).Description

    member x.Inventory () =
            let sb = new StringBuilder()
            ignore <| sb.AppendLine("You are carrying:")
            for o in inventory do
                ignore <| sb.AppendLine(sprintf "  %s" (GetObject o).Description)
            printfn "%s" <| sb.ToString()

    member x.Help () = 
            let sb = new StringBuilder()
            ignore <| sb.AppendLine("The following commands are accepted:")
            for c in commands do
                ignore <| sb.AppendLine(sprintf "  %s" c.Key)
            printfn "%s" <| sb.ToString()
            

    member x.Exit () = exit <- true
           
    

[<EntryPoint>]
let main argv = 
    let mutable exit = false
    let game = Game()
    while not exit do
        exit <- game.AcceptLine <| Console.ReadLine()

    0
