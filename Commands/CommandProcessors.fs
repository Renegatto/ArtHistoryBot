module CommandProcessors
open CommandHandler
open Commands
open Domain
open DomainTypes
open Errors
open Events
open Infrastructure

let nextTest (NextTest cmd): Asyncresult<DomainEvent list,Error> = asyncresult {
    let generator (TestGenerator gen) = gen
    let! test = generator cmd.generator ()

    return! Domain.nextTest cmd test |> Asyncresult.fromResult
}
let newTest (NewTest cmd): Asyncresult<DomainEvent list,Error> = asyncresult {
    let! artworks = MainIO.Storage.artworks ()
    let generator = MainIO.Randoms.testBuilder cmd.variants_count artworks >> IO.unwrapInsideAsync >> Asyncresult.ok
    let! test = generator ()

    return! Domain.newTest cmd (TestGenerator generator) test |> Asyncresult.fromResult
}

[<System.Diagnostics.DebuggerDisplay("Commands: command matching")>]
let matchCommand: CommandMatcher = 
    CommandMatcher (function
    //|GuessResult cmd -> cmd.sub_id,0
    //|NewTest cmd -> cmd.sub_id, newTest
    |NextTest cmd -> SubscriptionId cmd.sub_id, nextTest
    )