// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
module Program
open BotTest
open FSharpPlus

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let cmd = Commands.NewTest { 
        Commands.NewTestCommand.sub_id = 666
        Commands.NewTestCommand.variants_count = 2
    }
    let check_stuff = async {
        let! result = CommandProcessors.newTest cmd
        printfn "%A" result
    }
    Async.RunSynchronously check_stuff
    while true do
        //make_connection ()
        printfn "i'm still alive1 %A" ()
        System.Console.ReadLine ()
    printfn "i'm still alive2 %A" ()
    System.Console.ReadLine ()
    0 // return an integer exit code
