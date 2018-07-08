module Vector

type Vector = Vector of float[] with
    member v.Points = let (Vector v') = v in v'
    member v.Dimensions = Array.length v.Points
    member v.Divide d = Vector (Array.map (fun p -> p /  d) v.Points)
    static member Zero dimensions = Vector <| Array.create dimensions 0.0
    static member DistanceSquared (Vector v1) (Vector v2) =
        Array.fold2 (fun acc i ii -> ((i - ii) ** 2.0) + acc) 0.0 v1 v2
    static member Distance v1 v2 =
        (Vector.DistanceSquared v1 v2) |> sqrt
    static member Sum (Vector v1) (Vector v2) =
        Vector (Array.fold2 (fun acc i ii -> (i + ii) :: acc) [] v1 v2
        |> List.toArray
        |> Array.rev)
    static member Equal (Vector v1) (Vector v2) =
        Array.forall2 (=) v1 v2
