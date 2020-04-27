// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
module Program
open BotTest
open FSharpPlus

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


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let foo = List.map (fun _ -> MainIO.Randoms.element [0..20] ()) [0..30]

    printfn "%A" foo |> ignore

    test_commands () |> ignore
    (*let CM = 
        match CommandProcessors.matchCommand with
        |Commands.CommandMatcher x -> x

    let manuallyProcessCommand =  CM>>snd>>(fun f -> f ())

    let cmd1 = Commands.NewTest { 
        Commands.NewTestCommand.sub_id = 666
        Commands.NewTestCommand.variants_count = 3
    }
    let check_stuff = async {
        let! result = manuallyProcessCommand cmd1
        let guess events: Infrastructure.Asyncresult<Events.DomainEvent list,Errors.Error> = 
            match List.rev events |> List.head with
            |Events.TestSended x ->
                Commands.GuessResult {
                    Commands.GuessResultCommand.answer = 2
                    Commands.GuessResultCommand.sub_id = x.sid
                    Commands.GuessResultCommand.test = x.test
                } |> manuallyProcessCommand
        let next_quiz events =
            match List.head events with
            |Events.NewQuizStarted x -> Commands.NextTest { 
                Commands.NextTestCommand.sub_id = x.sid
                Commands.NextTestCommand.generator = x.generator
            } |> manuallyProcessCommand

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
        Async.RunSynchronously check_stuff*)
    
    while true do
        //make_connection ()
        printfn "i'm still alive1 %A" () |> ignore
        System.Console.ReadLine () |> ignore
    printfn "i'm still alive2 %A" ()
    System.Console.ReadLine () |> ignore
    0 // return an integer exit code
