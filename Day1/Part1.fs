module Day1.Part1

let solution () =
    let input = System.IO.File.ReadAllLines("input.txt") |> Seq.ofArray
    
    let depthPairs =
        input
        |> Seq.map int // I have no clue why casting to int matters here but it does
        |> Seq.pairwise
        
    let countIncreasing =
        depthPairs
        |> Seq.map (fun (a, b) -> a < b)
        |> Seq.filter id
        |> Seq.length

    printfn $"Number of increasing depth pairs: {countIncreasing}"