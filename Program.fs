// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let resA = Assignments.count [1;1;2;2;2;3;3;3;3;4;4;4;4;4] 1
    printfn "Result A: %A" resA

    let resB = Assignments.insert [1;2;4;5] 3
    printfn "Result B: %A" resB

    let resC = Assignments.intersect [1;1;1;2;2] [1;1;2;4]
    printfn "Result C: %A" resC

    let resD = Assignments.plus [1;1;2] [1;2;4]
    printfn "Result D: %A" resD

    let resE = Assignments.minus [1;1;1;2;2]  [1;1;2;3] 
    printfn "Result E: %A" resE
    0 
