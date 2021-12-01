module Day1.Part2

let rec createWindows measurements =
    match measurements with
    | [] | [ _ ] | [ _; _ ] -> failwith "List must have at least three elements"
    | [ a; b; c ]   -> [ a + b + c ]
    | a::b::c::tail -> a + b + c :: (createWindows <| b :: c :: tail)

let solution fileName =
    let count =
        readListOfInts fileName
        |> createWindows
        |> countIncreasing
    
    printfn $"Number of increasing depth pairs: {count}"