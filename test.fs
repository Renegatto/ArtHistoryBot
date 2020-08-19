module Test
open DomainTypes
open Domain
open Infrastructure

let mutable eventHub : (SubscriptionId * Events.DomainEvent) list = [] 

let eventPublisher : Commands.EventPublisher = 
    Commands.EventPublisher <|
    fun (events_info : (SubscriptionId * Events.DomainEvent) list) -> 
        eventHub <- List.append eventHub events_info
        async { return () }
let commMatcher : Commands.CommandMatcher = CommandProcessors.matchCommand
let commHandler = CommandHandler.CommandHandler.handle eventPublisher commMatcher

let eventsOf sid = 
    List.filter (fst >> (=) sid) eventHub
    |> List.unzip |> snd

let findTest (sid:SubscriptionId) : Test = // option =
    let test = function
        |Events.TestSolved event -> Some event.test
        |Events.TestFailed event -> Some event.test
        |Events.NewQuizStarted _ -> None
        |Events.TestSended event -> Some event.test
    eventsOf sid
    |> List.rev
    |> List.pick test

let commandsToHandle : Commands.Command list = [
    Commands.NewTest {variants_count = 4};
    //Commands.GuessResult { Commands.GuessResultCommand.test  }
]

let testThisShit () : Asyncresult<string,Errors.Error> = asyncresult {
    let! sub = MainIO.Subscriptions.subscribe ()
    let sid = sub.sid
    let ch = commHandler sid

    let! events1 = ch <| Commands.NewTest {variants_count = 5}
    let gen = List.head events1 |> (function (Events.NewQuizStarted event) -> event.generator)


    let! events2 = ch <| Commands.GuessResult {test = findTest sid; answer = 2}

    let! events3 = ch <| Commands.NextTest { generator = gen }

    let! events4 = ch <| Commands.GuessResult {test = findTest sid; answer = 1}

    let! events5 = ch <| Commands.NextTest { generator = gen }

    let! events6 = ch <| Commands.GuessResult {test = findTest sid; answer = 3}
    
    let! events1' = ch <| Commands.NewTest {variants_count = 5}
    let gen' = List.head events1' |> (function (Events.NewQuizStarted event) -> event.generator)

    let! events2' = ch <| Commands.GuessResult {test = findTest sid; answer = 2}

    let! events3' = ch <| Commands.NextTest { generator = gen }

    let bazz _ = sprintf "testing result event hub: %A" eventHub

    printfn "%s" <| bazz events3'

    return (bazz events3')
}
    