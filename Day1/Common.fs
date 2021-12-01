[<AutoOpen>]
module Day1.Common

let readListOfInts fileName =
    System.IO.File.ReadAllLines(fileName)
    |> List.ofArray
    |> List.map int // I have no clue why casting to int matters here but it does

let countIncreasing list =
    list
    |> List.pairwise
    |> List.map (fun (a, b) -> a < b)
    |> List.filter id
    |> List.length