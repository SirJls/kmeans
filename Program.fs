open System
open Vector
open Logic
open Kmeans
open FSharp.Data

let readData (path : string) =
    let csv = CsvFile.Load(path, hasHeaders = false)
    csv.Rows
    |> Seq.map (fun r -> r.Columns |> List.ofArray) |> List.ofSeq

let toFloat m =
    List.map (fun x -> List.map System.Double.Parse x) m

let rec transpose m =
    match m with
        | (_::_)::_ as m' -> List.map List.head m' :: transpose (List.map List.tail m')
        | _ -> []

let matrixToObservations m : Observations =
    List.map (Vector << List.toArray) m |> List.toArray

let makeObservations = transpose >> matrixToObservations

[<EntryPoint>]
let main argv =
    let rawM = readData "../../../data/dataset.dat"
    let obs = toFloat rawM |> makeObservations
    let kmeans = Kmeans.New 4 100 0.01
    printfn "SSE: %.2f\n" (kmeans.Run obs |> printClusters)
    0 // return an integer exit code
