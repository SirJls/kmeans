# F# KMEANS CLUSTERING

*** Cluster your customer / wine dataset ***

## Description

This is an F# implementation of the Cluster analysis exercise from the book "Data Smart" by John W. Foreman.
This implementation uses the provided wine dataset from the book.

This implementation allows for thee different distance measures:

* EuclideanDistance (default)
* ManhattanDistance
* CosineDistance

And it uses the silhouette method to measure how well each observation has been classified, in order to judge each clustering result.

This program has one entry point: Program.fs.
This entry point includes two imports, Utils & Kmeans.

Utils will allow us to print relevant cluser information after the algorithm
has converged, e.g: classifications (all centroids along with their observations) and the classification score (using silhouette).

Kmeans provides us with an interface to tweak/optimize the algorithm. We can
tweak the following parameters:

Kmeans:
* K: <- number of clusters 
* Threshold <- amount of iterations/cycles before stopping 
* Delta <- premature converge the algorithm, by calculating the delta between
the current cycle and the previous cycle and comparing it with the provided
value, if this is smaller converge, otherwise continue.


## Dependencies

### Software
* FSharp.Data

### Tooling
* Paket version >= 5.219.0 (to install follow these [steps](https://fsprojects.github.io/Paket/paket-and-dotnet-cli.html))
* [.NET core](https://docs.microsoft.com/en-us/dotnet/core/install/sdk?pivots=os-windows) version >= 2.2.103 
* [Mono](https://www.mono-project.com/docs/getting-started/install/windows) version >= 5.16.0.220 


## How to execute the program

1) Install all the dependencies, start with installing the necessary tooling (If you clone this repository paket will be available in the hidden folder.
2) Pull in the dependencies by executing paket.exe.
3) Execute the the algorithm, use the Kmeans.Run interface and provide observation parameters, as shown below:

To run the algorithm, issue the following commands from your CLI:
<br/>
```mono ./.paket/paket.exe install```
<br/>
```dotnet run -- /full/path/to/the/data/dataset.dat```

MAKE SURE YOU PULLED IN THE DEPENDENCIES, INSTALL PAKET, as listed in [Tooling](### Tooling)
