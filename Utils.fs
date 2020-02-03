module Utils

open Vector
open Logic
open Kmeans
open FSharp.Data

let euclideanDistance = EuclideanDistance (fun (Vector v1) (Vector v2) -> sqrt <| (Array.fold2 (fun acc i ii -> ((i - ii) ** 2.0) + acc) 0.0 (snd v1) (snd v2)))
let manhattanDistance = ManhattanDistance (fun (Vector v1) (Vector v2) -> (Array.fold2 (fun acc i ii -> (abs (i - ii)) + acc) 0.0 (snd v1) (snd v2)))
let consineDistance = CosineDistance (fun (Vector v1) (Vector v2) ->
                                       let sum = Array.fold2 (fun acc i ii -> i * ii) 0.0 (snd v1) (snd v2)
                                       let a = Array.sumBy (fun x -> x ** 2.0) (snd v1) |> sqrt
                                       let b = Array.sumBy (fun x -> x ** 2.0) (snd v2) |> sqrt
                                       in sum / (a * b))

let readCSV (path : string) =
    let csv = CsvFile.Load(path, hasHeaders = false)
    csv.Rows
    |> Seq.map (fun r -> r.Columns |> List.ofArray) |> List.ofSeq

let toFloat m =
    List.map (fun x -> List.map System.Double.Parse x) m

let rec transpose m =
    match m with
        | (_::_)::_ as m' -> List.map List.head m' :: transpose (List.map List.tail m')
        | _ -> []

let matrixToObservations (df : DistanceFunc) (m : float list list) =
    List.map ((fun l -> Vector (df, l)) << List.toArray) m |> List.toArray

let makeObservations (df : DistanceFunc) = transpose >> (matrixToObservations df)

let printClusterInfo (cl : ClusterAndCycle) (clusterNumber : int) (cls : ClustersAndCycle) =
    let ((ce, obs), currentCycle, maxCycle) = cl

    printfn "======================================\n"
    printfn "KMeans converged in: %d/%d cycles." currentCycle maxCycle
    printfn "Cluster: %d." clusterNumber
    printfn "Centroid: %A." ce.Points
    printfn "Average silhouette for cluster %d: %.2f."  clusterNumber ((Array.fold (fun acc x -> let _,_,s = (silhouette x cls) in acc + s) 0.0 obs) / float(Array.length obs))


    let offers = Array.map (fun (ob : Observation) ->
                       let z = Array.mapi (fun i x -> (i, x)) ob.Points in
                            let r = Array.fold (fun acc (i, x) -> if x = 1.0 then (i, x) :: acc else acc) [] z
                            List.toArray r
                       ) obs

    let offers' = Array.concat <| Array.map (fun offer -> Array.countBy (fun (offer', _) -> offer') offer) offers
    let offers'' = Array.countBy (fun (offer, _) -> offer) offers'

    // let r = Array.map (fun (ob : Observation) -> (Array.mapi (fun i x -> (i, x)) ob.Points)) obs

    Array.iter (
        fun (offer, times) ->
        printfn "Offer %d taken: %d times!" offer times) <| Array.sort offers''
    // printfn "%A" offers''

    // let obs' = Array.map (fun (v : Vector) -> Array.toList v.Points) obs |> Array.toList

    // let r = transpose obs'

    // printfn "%A" r

    // let rec toVector (m : 'a list list) =
    //     match m with
    //     | (x::_)::xss -> x :: (toVector xss)
    //     | _ -> []

    // printfn "Observation details:"

    // let print cl =
    //     Array.iter (
    //         fun (ob : Observation) ->
    //         let (a, b, s) = silhouette ob cls
    //         printfn "--------------------------------------\n"
    //         printfn "Details for observation %A:" ob.Points
    //         printfn "The cluster assignment value is:\n %.2f." a
    //         printfn "The average distance to the neighbouring cluster with centroid %.2f" b
    //         // printfn "The average distance to the neighbouring cluster with centroid %A:\n %.2f" b
    //         printfn "The silhouette value is:\n %.2f." s) cl

    // print obs
    // printfn


let printClustersInfo (cls : ClustersAndCycle) =
    let clusters, currentCycle, maxCycle = cls
    Array.iteri (fun i cl -> printClusterInfo (cl,currentCycle,maxCycle) (i+1) cls ) clusters

    let ssum = Array.map (fun (_, obs) -> (Array.fold (fun acc x -> let _,_,s = (silhouette x cls) in acc + s) 0.0 obs) / float(Array.length obs)) clusters
    let ssilhouette = (Array.sum ssum) / float(Array.length clusters)

    printfn "\nTotal silhouette: %.2f" ssilhouette
    printfn "SSE: %.2f" (sse clusters)
