module EventHandler

open Infrastructure
open Errors
open Events

type CommandPublisher = CommandPublisher of (Commands.Command list -> unit Async) //cycle depend.

type Events = Events of Events []

type DomainEventProcessor   = unit -> Asyncresult<Commands.Command list,Error>
type ExternalEventProcessor = unit -> Asyncresult<Commands.Command list,Error>

type DomainEventMatcher = 
    DomainEventMatcher of (DomainEvent -> DomainTypes.SubscriptionId * DomainEventProcessor)
type ExternalEventMatcher = 
    ExternalEventMatcher of (ExternalEvent -> DomainTypes.SubscriptionId * ExternalEventProcessor)