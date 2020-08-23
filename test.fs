module Test
open DomainTypes
open Domain
open Infrastructure


let ( >=> ) f g = Asyncresult.andThen f g
let test = EventHub.lastTestOf
let generator = EventHub.lastTestGeneratorOf
let ( *>) a b = Asyncresult.next a b
let ( >*>) a b x = a x *> b x
let putLine s x: Asyncresult<_,_> = printfn "%s %A" s x; Asyncresult.ok x
let sid = SubscriptionId
let sentMessage id msg sid': SubscriptionId * Events.Event = 
    Events.UserSentMessage {user_id = id; message = msg}
    |> Events.External
    |> fun x -> sid sid',x

let testThisShit () : Asyncresult<unit,Errors.Error> = 
    let commandHub = new CommandHub.CommandHub()
    let eventHub = new EventHub.EventHub()
    let commandHandler = CurrentConfiguration.CommandHandler(eventHub,commandHub)
    let eventHandler = CurrentConfiguration.EventHandler(eventHub,commandHub)
    eventHandler.subscribe()
    commandHandler.subscribe()

    asyncresult {
        let! one = eventHub.push(sentMessage 228 "new 5 2" 55) 
        let! one' = eventHub.push(sentMessage 228 "answer 2 ggg" 55)
        let! one'' = eventHub.push(sentMessage 228 "answer 7 ggg" 55)
        let! one''' = eventHub.push(sentMessage 228 "answer 3 ggg" 55)
        printfn "Уильям, блядь"
        return ()
    }
(*
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
*)