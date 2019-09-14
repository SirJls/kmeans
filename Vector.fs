module Vector

open System

[<CustomEquality; CustomComparison>]
type DistanceFunc =
    | EuclideanDistance of (Vector -> Vector -> float)
    | ManhattanDistance of (Vector -> Vector -> float)
    | ConsineDistance of (Vector -> Vector -> float)
    override x.Equals y =
        match y with
            | :? DistanceFunc as y' ->
                match (x, y') with
                    | (EuclideanDistance _, EuclideanDistance _) -> true
                    | (ManhattanDistance _, ManhattanDistance _) -> true
                    | (ConsineDistance _, ConsineDistance _) -> true
                    | _ -> false
            | _ -> false
    override x.GetHashCode() =
        match x with
            | (EuclideanDistance y) -> 1
            | (ManhattanDistance y) -> 2
            | (ConsineDistance y) -> 3
    interface System.IComparable with
        member x.CompareTo y =
            match y with
                | :? DistanceFunc as y' ->
                    match (x, y') with
                        | (EuclideanDistance _, EuclideanDistance _) -> 1
                        | (ManhattanDistance _, ManhattanDistance _) -> 2
                        | (ConsineDistance _, ConsineDistance _) -> 3
                        | _ -> failwith "wrong type"
                    | _ -> failwith "wrong type"

and Vector = Vector of (DistanceFunc * float[]) with
    member v.Points =
        match v with
            | Vector (_, v') -> v'
    member v.DistanceFunc =
        match v with
            | Vector (f, _) -> f
    member v.Dimensions = Array.length v.Points
    member v.Divide d =
        match v with
            | Vector (f, v') -> Vector (f, (Array.map(fun p -> p / d) v'))
    static member RetrieveAndCompareMetric (v1 : Vector) (v2 : Vector) =
        match (v1.DistanceFunc, v2.DistanceFunc) with
            | (EuclideanDistance f1, EuclideanDistance f2) ->
                Some (f1)
            | (ManhattanDistance f1, ManhattanDistance f2) ->
                Some (f1)
            | (ConsineDistance f1, ConsineDistance f2) ->
                Some (f1)
            | _ -> None
    static member Zero (f : DistanceFunc) dimensions = Vector <| (f, Array.create dimensions 0.0)
    static member DistanceSquared (Vector (_, v1)) (Vector (_, v2)) =
        Array.fold2 (fun acc i ii -> ((i - ii) ** 2.0) + acc) 0.0 v1 v2
    static member Distance v1 v2 =
        match Vector.RetrieveAndCompareMetric v1 v2 with
            | Some f -> f v1 v2
            | None -> failwith "Invalid or non-matching distance function."
    static member Sum v1 v2 =
        match Vector.RetrieveAndCompareMetric v1 v2 with
            | Some _ -> Vector (v1.DistanceFunc, Array.fold2 (fun acc i ii -> (i + ii) :: acc) [] v1.Points v2.Points
                                |> List.toArray
                                |> Array.rev)
            | None -> failwith "Invalid or non-matching distance function."
    static member Equal (v1 : Vector) (v2 : Vector) = Array.forall2 (=) v1.Points v2.Points

