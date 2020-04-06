module Commands
open Errors
open Domain
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