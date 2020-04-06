module CommandHandler
open Errors
open Domain
open Infrastructure
open MainIO
open Events

type GuessResultCommand = {
    sub_id : Subscriptions.SubscriptionId
}
type NewtTestCommand = {
    sub_id : Subscriptions.SubscriptionId
}
type NextTestCommand = {
    sub_id : Subscriptions.SubscriptionId
}
type Command = 
    |GuessResult of GuessResultCommand
    |NewTest of NewtTestCommand
    |NextTest of NextTestCommand

type EventPublisher = EventPublisher of (DomainEvent -> unit Async)
type Commands = Commands of Command []
type CommandProcessor = (Command -> Result<DomainEvent,Error>)
type CommandMatcher = CommandMatcher of (Command -> Subscriptions.SubscriptionId*CommandProcessor)

type CommandHandler = CommandHandler with //CommandHandler of (Command -> Result<DomainEvent,Error>)
    static member private handleCommand (command:Command) 
        (EventPublisher publish) 
        (CommandMatcher matcher)
        : Asyncresult<DomainEvent,Error> = asyncresult {

        let (sid,processor) = matcher command
        let! stored_data = Subscriptions.readData sid
        let event: Result<DomainEvent,Error> = processor command // stored_data

        Result.map publish event |> ignore

        return! event |> Asyncresult.fromResult
    }
    static member handle = CommandHandler.handleCommand
    

let mutable commands = Commands Array.empty