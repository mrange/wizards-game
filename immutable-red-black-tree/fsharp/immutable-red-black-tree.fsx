// From Purely functional datastructures by Chris Okasaki

module RedBlackTree =
    open System

    type Color =
        | Red
        | Black

    type Tree<'TKey, 'TValue when 'TKey :> IComparable<'TKey>> =
        | Node  of Color*'TKey*'TValue*Tree<'TKey, 'TValue>*Tree<'TKey, 'TValue>
        | Empty

    let rec tryFind (k : 'TKey) (t : Tree<'TKey, 'TValue>) =
        match t with
        | Empty             ->  None
        | Node (_,kk,v,l,r) ->  match k.CompareTo(kk) with
                                | 0             -> Some v
                                | n when n < 0  -> tryFind k l
                                | _             -> tryFind k r

    module Details =
        let lbalance (t : Tree<'TKey, 'TValue>) =
            match t with
            | Node (Black, zk, zv, Node (Red, yk, yv, Node (Red, xk, xv, a, b), c), d)
            | Node (Black, zk, zv, Node (Red, xk, xv, a, Node (Red, yk, yv, b, c)), d)  ->
                    Node (Red, yk, yv, Node (Black, xk, xv, a, b), Node (Black, zk, zv, c, d))
            | _ -> t

        let rbalance (t : Tree<'TKey, 'TValue>) =
            match t with
            | Node (Black, xk, xv, a, Node (Red, zk, zv, Node (Red, yk, yv, b, c), d))
            | Node (Black, xk, xv, a, Node (Red, yk, yv, b, Node (Red, zk, zv, c, d)))  ->
                    Node (Red, yk, yv, Node (Black, xk, xv, a, b), Node (Black, zk, zv, c, d))
            | _ -> t

        let rec addOrReplace k v t =
            match t with
            | Empty                     ->  Node (Red, k, v, Empty, Empty)
            | Node (c, kk ,vv ,l ,r)    ->  match k.CompareTo(kk) with
                                            | 0             -> Node (c, kk, v, l, r)
                                            | n when n < 0  -> Node (c, kk, vv, addOrReplace k v l, r) |> lbalance
                                            | _             -> Node (c, kk, vv, l, addOrReplace k v r) |> rbalance


    let addOrReplace k v (t : Tree<'TKey, 'TValue>) =
        match Details.addOrReplace k v t with
        | Node (_, kk ,vv, l, r)    -> Node (Black, kk ,vv, l, r)
        | _                         -> failwith "impl should always return a tree"

    let rec length (t : Tree<'TKey, 'TValue>) =
        match t with
        | Empty                 -> 0
        | Node (_, _ ,_ ,l ,r)  -> 1 + (l |> length) + (r |> length)

    let rec depth (t : Tree<'TKey, 'TValue>) =
        match t with
        | Empty                 -> 0
        | Node (_, _ ,_ ,l ,r)  -> 1 + max (l |> depth) (r |> depth)

    let ofSeq (vs : seq<'TKey*'TValue>) =
        let mutable t = Empty
        for k,v in vs do
            t <- t |> addOrReplace k v
        t

let test testRun (testSet : int list) =

    printfn "Running test run: %s" testRun

    let log2 v = (v |> float |> log) / log 2.
    let asInt v = round v |> int

    let unique = testSet |> Seq.distinct |> List.ofSeq

    let rbt = testSet |> Seq.map (fun v -> v,()) |> RedBlackTree.ofSeq

    for v in testSet do
        match rbt |> RedBlackTree.tryFind v with
        | None  -> printfn "FAILED: %d not found in tree" v
        | _     -> ()

    let length = rbt |> RedBlackTree.length
    if unique.Length <> length then
        printfn "FAILED: Length mismatch: %d <> %d" unique.Length length


    let depth = rbt |> RedBlackTree.depth
    let minDepth = length |> log2 |> asInt
    let maxDepth = 2. * (length + 1 |> log2) |> asInt

    if minDepth > depth then
        printfn "FAILED: minDepth exceeds depth"

    if maxDepth < depth then
        printfn "FAILED: maxDepth is lower than depth"

    rbt

open System

let r = Random 19740531
printfn "%A" <| test "Manual" [31;41;59;26;53;58;97;93]
for testRun in 0..10 do
    let length = r.Next(1000,2000)
    ignore <| test (sprintf "Generated: %d" testRun) [for i in 0..length -> r.Next()]
