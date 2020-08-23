// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
module Program
//open BotTest
open FSharpPlus
(*
let goo () = 
    let t = new System.Timers.Timer(4000.0)
    t.Elapsed.Add(fun _ -> printfn "%A" EventHub.Internals.eventHub)
    t.AutoReset <- false
    t.Start()

let test_commands () =
    let publisher xs = async { return () }
    let CommandHandler cmd = 
        (Commands.EventPublisher publisher) 
        |> CommandHandler.CommandHandler.handle cmd 
        <| CommandProcessors.matchCommand

    let cmd1 = Commands.NewTest { 
        Commands.NewTestCommand.sub_id = 666
        Commands.NewTestCommand.variants_count = 3
    }

    let check_stuff = async {
        let! result = CommandHandler cmd1
        let guess events: Infrastructure.Asyncresult<Events.DomainEvent list,Errors.Error> = 
            match List.rev events |> List.head with
            |Events.TestSended x ->
                Commands.GuessResult {
                    Commands.GuessResultCommand.answer = 2
                    Commands.GuessResultCommand.sub_id = x.sid
                    Commands.GuessResultCommand.test = x.test
                } |> CommandHandler
        let next_quiz events =
            match List.head events with
            |Events.NewQuizStarted x -> Commands.NextTest { 
                Commands.NextTestCommand.sub_id = x.sid
                Commands.NextTestCommand.generator = x.generator
            } |> CommandHandler

        let! guessresult = 
            Infrastructure.Asyncresult.fromResult result 
            |> Infrastructure.Asyncresult.bind guess
        let! nexttest = 
            Infrastructure.Asyncresult.fromResult result 
            |> Infrastructure.Asyncresult.bind next_quiz
        let! nextresult = 
            Infrastructure.Asyncresult.fromResult nexttest
            |> Infrastructure.Asyncresult.bind guess
        let! nexttest2 = 
            Infrastructure.Asyncresult.fromResult result 
            |> Infrastructure.Asyncresult.bind next_quiz
        let! nextresult2 = 
            Infrastructure.Asyncresult.fromResult nexttest2
            |> Infrastructure.Asyncresult.bind guess

        printfn "%A \n--------------and result------------ \n %A" result guessresult
        printfn "\n================== \n"
        printfn "%A \n--------------and result------------ \n %A" nexttest nextresult
        printfn "\n================== \n"
        printfn "%A \n--------------and result------------ \n %A" nexttest2 nextresult2
    }
    for x in [0..10] do
    Async.RunSynchronously check_stuff
*)


open Infrastructure

let asyncresultMap f = Asyncresult.bind  (Asyncresult.ok << f)
//let asyncresultSmthng f = f << Asyncresult.ok

let testTestGeneration () = 
    let foo = List.map (fun _ -> MainIO.Randoms.element [0..20] ()) [0..30]

    let artworks = MainIO.Storage.artworks ()

    let bar = 
        let fn (): Asyncresult< Result<DomainTypes.Test IO,Errors.Error>, Errors.Error > = asyncresult {
            let! artworks' = artworks
            return MainIO.Randoms.testBuilder 5 artworks' ()//|> IO.map (List.distinct >> List.length)
        }
        fn >> Async.RunSynchronously

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
        (*Asyncresult.flatten b
        |> Asyncresult.bind f
        |> Asyncresult.map (printfn "result of internal: %A")
        Asyncresult.next
            (printfn "foo" |> Asyncresult.ok)
            (printfn "bar" |> Asyncresult.ok)*)

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
    
    printfn "now..."

    Foo.foo () |> Async.RunSynchronously |> printfn "%A"

    Test.testThisShit () |> Async.RunSynchronously
    |> ignore
    
    (*(async { 
       // return! MainIO.Storage.initialize ()
        let! cont = MainIO.Storage.artworks ()
        return printfn "artworks: %A" cont 
    }) |> Async.Start |> ignore*)
    //goo ()
    // |> printfn "eventHub: %A %A" EventHub.Internals.eventHub
    printfn "called..."
    
    while true do
        //make_connection ()
        printfn "i'm still alive1 %A" () |> ignore
        System.Console.ReadLine () |> ignore
    printfn "i'm still alive2 %A" ()
    System.Console.ReadLine () |> ignore
    0 // return an integer exit code
