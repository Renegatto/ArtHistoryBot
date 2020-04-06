module Commands
open CommandHandler
open Infrastructure
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

[<System.Diagnostics.DebuggerDisplay("Commands: command matching")>]
let matchCommand = function
    |GuessResult cmd -> cmd.sub_id,0
    |NewTest cmd -> cmd.sub_id,0
    |NextTest cmd -> cmd.sub_id,0