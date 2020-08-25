module EventHandler

open Infrastructure
open Errors
open Events

type R<'a> = AResult<'a,Errors.Error>
type R'<'a> = Result<'a,Errors.Error>
type StoredCommand = (DomainTypes.SubscriptionId * Commands.Command)
type StoredCommands = StoredCommand list
type Commands = Commands.Command list
type StoredEvent = (DomainTypes.SubscriptionId * Event)
type StoredEventFold<'a> = StoredEvent list -> 'a
type CommandPublisher = 
    |CommandPublisher of (StoredCommand list 
         -> Asyncresult<unit,Error>) //cyclic dependendency.

//type Events = Events of Events list

type EventProcessor = 
    DomainTypes.SubscriptionId -> Commands R' StoredEventFold
type DomainEventProcessor   = 
    DomainTypes.SubscriptionId -> Commands R' StoredEventFold
type ExternalEventProcessor = 
    DomainTypes.SubscriptionId -> Commands R' StoredEventFold

type DomainEventMatcher = 
    DomainEventMatcher      of (DomainEvent -> DomainEventProcessor)
type ExternalEventMatcher = 
    ExternalEventMatcher    of (ExternalEvent -> ExternalEventProcessor)
type EventMatcher = 
    EventMatcher            of (Event -> EventProcessor)
//open FSharpPlus
let handle
    (EventMatcher processor)
    (sid: DomainTypes.SubscriptionId)
    (event:Event)
    : StoredCommands R' StoredEventFold = 
    
    let catchError commands = 
        match commands with
        |Error (Errors.Domain error) -> Ok [Commands.ShowError error]
        |_ -> commands
    
    fun events -> result {

        let! commands = processor event sid events |> catchError

        let identified_commands = List.map (fun command -> sid,command) commands 
        return identified_commands
    }
