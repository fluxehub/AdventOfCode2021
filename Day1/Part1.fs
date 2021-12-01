module Day1.Part1

let solution fileName =
    let count =
        readListOfInts fileName
        |> countIncreasing
        
    printfn $"Number of increasing depth pairs: {count}"