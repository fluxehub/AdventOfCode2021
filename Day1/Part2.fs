module Day1.Part2

let solution fileName =
    let count =
        readListOfInts fileName
        |> List.windowed 3
        |> List.map List.sum
        |> countIncreasing
    
    printfn $"Number of increasing depth windows: {count}"