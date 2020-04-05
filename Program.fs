// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
module Program
open BotTest
open FSharpPlus

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    while true do
        //make_connection ()
        printfn "i'm still alive1 %A" ()
        System.Console.ReadLine ()
    printfn "i'm still alive2 %A" ()
    System.Console.ReadLine ()
    0 // return an integer exit code
