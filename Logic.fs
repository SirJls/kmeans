module Logic

open System
open Vector

type Observation = Vector

type Centroid = Observation

type Centroids = Centroid[]

type Observations = Observation[]

type Cluster = Centroid * Observations

type Clusters = Cluster[]

type CurrentCycle = int

type MaxCycle = int

type ClusterAndCycle = Cluster * CurrentCycle * MaxCycle

type ClustersAndCycle = Clusters * CurrentCycle * MaxCycle

let forgy (k : int) (obs : Observations) : Centroids =
    if k > obs.Length && k < 0 then
        failwith "K should be bigger than 0 and at least be equal to the amount of observations!"
    else
        let r = Random()
        [|for i in [1..k] do yield obs.[r.Next(0, obs.Length-1)]|]

let near (cts: Centroids) (obs : Observations) =
    Array.map (fun (ob : Observation) ->
               let (ce,ob',_) = [|
                                 for ct in cts -> (ct, ob, Vector.Distance ct ob)
                                |] |> Array.minBy (fun (_,_,d) ->
                                                   d) in ce,ob') obs

let equals (a : Observations) (b : Observations) =
    Array.forall2 (Vector.Equal) a b

let extractCentroids (cls : Clusters) : Centroids =
    Array.map (fun (ce,_) -> ce) cls

let sse (cls : Clusters) =
    Array.sumBy (fun (ce, obs) ->
                  Array.fold (fun acc ob ->
                          (Vector.DistanceSquared ce ob) + acc ) 0.0 obs) cls

let printCluster (cl : ClusterAndCycle) =
    let ((ce, obs), currentCycle, maxCycle) = cl

    let cycle = (maxCycle - currentCycle)

    printfn "Clycles %d, with Centroid: %A\n" cycle ce.Points
    printfn "Observations assigned to Centroid:\n\n"

    let print a =
        Array.iter (fun (ob : Observation) -> printfn "%A" ob.Points) a

    print obs
    printf "\n"

let printClusters (cls : ClustersAndCycle) =
    let clusters, currentCycle, maxCycle = cls
    Array.iter (fun cl -> printCluster (cl,currentCycle,maxCycle) ) clusters
    sse clusters

let computeCentroid (cl : Cluster) : Centroid =
    let (ce,obs) = cl
    let (zero : Observation) = Vector.Zero ce.Dimensions
    let (centroid : Centroid) = Array.fold (fun acc x ->
                                            (Vector.Sum acc x)) zero obs
    centroid.Divide (float centroid.Dimensions)

let computeCentroids (cls : Clusters) =
    Array.map (computeCentroid) cls

let computeClusters (cts : Centroids) (obs : Observations) =
    let assigned  = near cts obs
    let sorted = assigned |> Array.groupBy (fun (ce,_) -> ce)
    let clusters : Clusters = sorted |> Array.map (fun (k,v) ->
                                        k, v
                                        |> Array.collect (fun (_,ob) ->
                                                          [|ob|]))
    clusters

let centroidsDelta (a : Centroids) (b : Centroids) (delta : float) =
    Array.map2 Vector.Distance a b |> Array.forall (fun i -> i < delta)

let cluster (obs : Observations) (k : int) (threshold : int) (delta : float) =
    let clusters = computeClusters (forgy k obs) obs
    let maxCycle = threshold
    let currentCycle = maxCycle

    let rec cluster' (cls : ClustersAndCycle) =
        let clusters', currentCycle', maxCycle' = cls
        if currentCycle' = 0 then
            cls
        else
            let centroids = extractCentroids clusters'
            let centroids' = computeCentroids clusters'
            let clusters'' = computeClusters centroids' obs

            if equals centroids centroids' then
                clusters'',currentCycle',maxCycle'
            else if centroidsDelta centroids centroids' delta then
                clusters'',currentCycle',maxCycle'
            else
                cluster' (clusters'', (currentCycle' - 1),maxCycle')

    cluster' (clusters, currentCycle, maxCycle)
