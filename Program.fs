open System
open Utils
open Kmeans

[<EntryPoint>]
let main argv =
    let rawM = readCSV "../../../data/dataset.dat"
    let obs = toFloat rawM |> makeObservations euclideanDistance
    let kmeans = Kmeans.New 4 100 0.01
    let result = kmeans.Run obs
    result |> printClustersInfo
    0 // return an integer exit code
