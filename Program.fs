open System
open Utils
open Kmeans

[<EntryPoint>]
let main argv =
    if Array.length argv <> 1 then
        printfn "Please provide a path to the wine dataset"; 1
    else
        try
            let rawM = readCSV (Array.head argv)
            let obs = toFloat rawM |> makeObservations euclideanDistance
            let kmeans = Kmeans.New 4 100 0.01
            let result = kmeans.Run obs
            result |> printClustersInfo
            0
        with ex -> printfn "Could not find the target, please use a full path to target the dataset!"; 1
