module Test
open DomainTypes
open Domain
open Infrastructure

(*
let mutable eventHub : (SubscriptionId * Events.DomainEvent) list = [] 

let eventPublisher : Commands.EventPublisher = 
    Commands.EventPublisher <|
    fun (events_info : (SubscriptionId * Events.DomainEvent) list) -> 
        eventHub <- List.append eventHub events_info
        async { return () }*)
let eventPublisher = 
    List.map (fun (sid,event) -> (sid,Events.Domain event)) 
    >> EventHub.publish
    |> Commands.EventPublisher
let commMatcher : Commands.CommandMatcher = CommandProcessors.matchCommand
let commHandler = CommandHandler.CommandHandler.handle eventPublisher commMatcher
(*
let eventsOf sid = 
    List.filter (fst >> (=) sid) eventHub
    |> List.unzip |> snd

let EventHub.lastTestOf (sid:SubscriptionId) : Test = // option =
    let test = function
        |Events.TestSolved event -> Some event.test
        |Events.TestFailed event -> Some event.test
        |Events.NewQuizStarted _ -> None
        |Events.TestSended event -> Some event.test
    eventsOf sid
    |> List.rev
    |> List.pick test *)
let ( >=> ) f g = Asyncresult.andThen f g
let test = EventHub.lastTestOf
let generator = EventHub.lastTestGeneratorOf
let ( *>) a b = Asyncresult.next a b
let ( >*>) a b x = a x *> b x
let putLine s x: Asyncresult<_,_> = printfn "%s %A" s x; Asyncresult.ok x
let testThisShit () : Asyncresult<'j,Errors.Error> = asyncresult {
    let! sub = MainIO.Subscriptions.subscribe ()
    let sid = sub.sid
    let ch = commHandler sid

    let ans variant test = Commands.GuessResult {test = test; answer = variant} |> ch
    let next gen = Commands.NextTest { generator = gen } |> ch

    //let! lasttest2 = 
    let myBeautifulComposeSpam = 
        ((Commands.NewTest {variants_count = 5} |> ch |> constant)
        >*> (test >=> ans 2) >*> (generator >=> next)
        >*> (test >=> ans 3) >*> (generator >=> next)
        >*> (test >=> ans 1) >*> (generator >=> next)
        >*> (Commands.NewTest {variants_count = 5} |> ch |> constant)
        >*> (test >=> ans 4) >*> (generator >=> next)
        >*> (test >=> ans 5) >*> (generator >=> next)
        >*> (putLine "ya konchiiiil!!!!!!!!!!!!%A--------------------" 7 |> constant)
        ) <| sid
    let bar = (putLine "hello" >=> putLine "oneMore?") 8
    let! foo = 
        (putLine "hello" >=> putLine "oneMore?")
        >*> putLine "world" 
        <| ()

    let bazz _ = sprintf "testing result event hub: %A" EventHub.Internals.eventHub

    //printfn "%s" <| bazz events3'
    
    return! myBeautifulComposeSpam// (Asyncresult.ok <| sprintf "----------------------")//  (bazz events3'))
}
    