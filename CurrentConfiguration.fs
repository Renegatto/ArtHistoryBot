module CurrentConfiguration
open Infrastructure
open Infrastructure.Operators

type R<'a> = AResult<'a,Errors.Error>
type R'<'a> = Result<'a,Errors.Error>
type StoredCommand = DomainTypes.SubscriptionId * Commands.Command
type StoredEvent = DomainTypes.SubscriptionId * Events.Event
type StoredEventsFold<'a> = StoredEvent list-> 'a
type StoredDomainEvent = DomainTypes.SubscriptionId * Events.DomainEvent

let handleCommand: StoredCommand -> StoredDomainEvent list R StoredEventsFold = 
    let fn = 
        CommandHandler.CommandHandler.handle 
          CommandProcessors.matchCommand
    in uncurry fn

let handleEvent: StoredEvent -> StoredCommand list R' StoredEventsFold = 
    let fn = 
        EventHandler.handle 
          EventProcessors.matchEvent
    in uncurry fn

let say s x =
    printfn s
    x
let asay s x =
    printfn s
    AResult.ok x
let show s y x =
    printfn s y;
    x
let ashow s y x =
    printfn s y;
    AResult.ok x

type EventHandler(eventHub,commandHub) =
    let unsubscribe: System.IDisposable list ref = ref []

    let eventHub:EventHub.EventHub = eventHub
    let commandHub:CommandHub.CommandHub = commandHub

    member o.updated(event) = 
         //printfn "......gen: handling event" |> ignore

         eventHub.processData (handleEvent event)
         //>>= ashow "handling event %A..." event
         >>= AResult.fromResult
         >>= commandHub.publish
         |> AResult.asyncEndpoint
         |> Async.Start
         |> ignore //|> ashow "EHan done (sync) %A" event |> ignore 

    interface System.IObserver<StoredEvent> with
        member _.OnCompleted() =
            //printfn "event handling has been completed"
            (List.head unsubscribe.contents).Dispose()

        member o.OnNext(event) = o.updated(event)

        member _.OnError(err) =
            printfn "event handling got an exception %A" exn

    member o.subscribe() =
        let eventHub: EventHub.EventHub = eventHub
        let commandHub: CommandHub.CommandHub = commandHub

        unsubscribe.contents <- [eventHub.Subscribe(o.updated)]

        ()

type CommandHandler(eventHub,commandHub) =
    let unsubscribe: System.IDisposable list ref = ref []
    
    //[<System.Obsolete("'actions' is not called from callback sometimes")>]
    member o.update(command) =
        let eventHub: EventHub.EventHub = eventHub
        let commandHub: CommandHub.CommandHub = commandHub

        //printfn ".. gen: handling command..."

        let actions x: unit R = 
            (*(printfn "handling command %A" x |> AResult.ok : unit R) *>*) (eventHub.publishDomain x:unit R)
 
        eventHub.processData (handleCommand command)
        |> join 
        >>= actions //nah, it's entering, here is all right
        |> AResult.asyncEndpoint
        |> Async.Start
        |> ignore //|> ashow "CHan done (sync) %A" command |> ignore

    interface System.IObserver<StoredCommand> with
        member _.OnCompleted() =
            printfn "command handling has been completed"
            (List.head unsubscribe.contents).Dispose()
        member o.OnNext(command) = o.update(command)

        member _.OnError(err) =
            printfn "command handling got an exception %A" exn

    member o.subscribe() =
        let eventHub: EventHub.EventHub = eventHub
        unsubscribe.contents <- [commandHub.Subscribe(o.update)]
        ()