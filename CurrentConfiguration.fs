﻿module CurrentConfiguration
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

type EventHandler(eventHub,commandHub) =
    let unsubscribe: System.IDisposable list ref = ref []

    let eventHub:EventHub.EventHub = eventHub
    let commandHub:CommandHub.CommandHub = commandHub

    member o.updated(event) = 
         printfn "handling event...%A" event
         eventHub.processData (handleEvent event)
         >>= AResult.fromResult
         >>= commandHub.publish
         |> AResult.toAsyncR
         |> Async.RunSynchronously
         |> ignore   

    interface System.IObserver<StoredEvent> with
        member _.OnCompleted() =
            printfn "event handling has been completed"
            (List.head unsubscribe.contents).Dispose()

        member o.OnNext(event) = o.updated(event)

        member _.OnError(err) =
            printfn "event handling got an exception %A" exn

    member o.subscribe() =
        let eventHub: EventHub.EventHub = eventHub
        let commandHub: CommandHub.CommandHub = commandHub

        unsubscribe.contents <- [eventHub.Subscribe(o.updated)]

        ()
let say s x =
    printfn s
    x
type CommandHandler(eventHub,commandHub) =
    let unsubscribe: System.IDisposable list ref = ref []
    
    [<System.Obsolete("'actions' is not called from callback sometimes")>]
    member o.update(command) =
        let eventHub: EventHub.EventHub = eventHub
        let commandHub: CommandHub.CommandHub = commandHub

        printfn "handling command...%A" command

        let actions x: unit R = 
            (printfn "inside: %A" x |> AResult.ok : unit R) *> (eventHub.publishDomain x:unit R)
 
        eventHub.processData (handleCommand command)
        |> say "now flatten..." |> join |> say "flattened"

        >>= actions //why this shit is not entering here?

        |> say "binded" 
        |> AResult.toAsyncR 
        |> ( *> ) <| Async.zero ()
        |> Async.Start
        |> ignore |> say "done"

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