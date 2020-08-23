// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
module Program
//open BotTest
open FSharpPlus
open Infrastructure

let testTestGeneration () = 
    let foo = List.map (fun _ -> MainIO.Randoms.element [0..20] ()) [0..30]

    let artworks = MainIO.Storage.artworks ()

    let bar = 
        let fn (): M< Result<DomainTypes.Test IO,Errors.Error>, Errors.Error > = aresult {
            let! artworks' = artworks
            return MainIO.Randoms.testBuilder 5 artworks' ()//|> IO.map (List.distinct >> List.length)
        }
        fn >> AResult.toAsyncR >> Async.RunSynchronously

    printfn "%A" foo |> ignore
    printfn "randoms %A randoms" <| List.map (fun _ -> bar ()) [0..5]
    ()
module Foo =
    open System.IO
    open System
    let foo () = 
        let a x = x * 2 |> Asyncresult.ok
        let f x = x + 5 |> Asyncresult.ok
        let b: Asyncresult<Asyncresult<_,_>,_> = Asyncresult.ok (a 5)
        Asyncresult.compose a f 7


[<EntryPoint>]
let main argv = 
    
    let baz = None
    (*let bar word potential_args = 
        let predicate (signature:Parser.Userinput.InstructionSignature) = 
            signature.name = word 
            && signature.args_count >= List.length potential_args
        List.filter predicate Parser.Userinput.signatures
        Infrastructure.option{
            let! matched_signature = List.filter predicate Parser.Userinput.signatures
            //let args_to_parse, remains = List.splitAt matched_signature.args_count potential_args
            return matched_signature//args_to_parse, remains
    }*)

    Parser.Userinput.instructions_from_line "new 5 3 hehe rakal 666 next answer 3 boogard"
    |> printfn "%A-%A--" baz
    printfn "%A" argv
    
    //printfn "now... %A, %A" (Test.baz <| Test.Foo 7) (Test.boo <| Test.Bar 9)

    Foo.foo () |> Async.RunSynchronously |> printfn "%A"

    Test.testThisShit () |> AResult.toAsyncR |> Async.RunSynchronously
    |> ignore

    printfn "called..."
    
    while true do
        //make_connection ()
        printfn "i'm still alive1 %A" () |> ignore
        System.Console.ReadLine () |> ignore
    printfn "i'm still alive2 %A" ()
    System.Console.ReadLine () |> ignore
    0 // return an integer exit code
