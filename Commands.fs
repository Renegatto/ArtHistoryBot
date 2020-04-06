module Commands
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
[<System.Diagnostics.DebuggerDisplay("Commands: command matching")>]
let matchCommand = function
    |GuessResult cmd -> cmd.sub_id,0
    |NewTest cmd -> cmd.sub_id,0
    |NextTest cmd -> cmd.sub_id,0
type EventPublisher = EventPublisher

type CommandHandler = CommandHandler with //CommandHandler of (Command -> Result<DomainEvent,Error>)
    static member private handleCommand (command:Command) (publish:EventPublisher): Asyncresult<DomainEvent,Error> = asyncresult {
        let (sid,processor) = matchCommand command
        let! stored_data = Subscriptions.readData sid
        let event: DomainEvent = processor command stored_data
        publish(event)
        return! event |> Asyncresult.fromResult
    }
    static member handle = CommandHandler.handleCommand
    
type Commands = Commands of Command []
type CommandMatcher = CommandMatcher of (Command -> CommandHandler)

let mutable commands = Commands Array.empty

open Infrastructure
module Command =
    open MainIO

    let nextTest (NextTest sid): Asyncresult<Test,Error> = asyncresult {
        match! Subscriptions.SubscriptionId sid |> Subscriptions.readData with
        |Subscriptions.TestData (TestGenerator test_generator) ->
            return! test_generator () |> Asyncresult.okAsync
        |Subscriptions.NoData ->
            return! TestGeneratorNotFounded sid |> Domain |> Asyncresult.error
    }
    let newTest (NextTest sid): Asyncresult<Test,Error> = asyncresult {// not impl
        match! Subscriptions.SubscriptionId sid |> Subscriptions.readData with
        |Subscriptions.TestData (TestGenerator test_generator) ->
            return! test_generator () |> Asyncresult.okAsync
        |Subscriptions.NoData ->
            return! TestGeneratorNotFounded sid |> Domain |> Asyncresult.error
    }