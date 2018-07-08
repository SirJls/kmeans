module Kmeans

type Kmeans =
    {
        k : int
        threshold : int
        delta : float
    }
    static member Default =
        { k = 4; threshold = 100; delta = 0.5 }
    static member New k threshold delta =
        { k = k; threshold = threshold; delta = delta }
    member m.Run (obs : Logic.Observations) =
        Logic.cluster obs m.k m.threshold m.delta
