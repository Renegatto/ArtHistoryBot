module Commands
open Errors
type DomainEvent = DomainEvent

type Command =
    |GuessResult
    |NewTest
    |NextTest of subscription_id:int
type CommandHandler = CommandHandler of (Command -> Result<DomainEvent,Error>)

type Commands = Commands of Command []
type CommandMatcher = CommandMatcher of (Command -> CommandHandler)

let mutable commands = Commands Array.empty

open Infrastucture
module Command =
    open MainIO
    let nextTest (NextTest sid) = 
        match Subscriptions.SubscriptionId sid |> Subscriptions.readData with
        |IO x -> result {
            match! x with
            |Subscriptions.TestData (TestGenerator x) ->
                
            |Subscriptions.NoData ->
        
        }