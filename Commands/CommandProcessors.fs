module CommandProcessors
open CommandHandler
open Commands
open Domain
open DomainTypes
open Errors
open Events
open Infrastructure

let nextTest (cmd:NextTestCommand): Asyncresult<DomainEvent list,Error> = asyncresult {
    let generator (TestGenerator gen) = gen
    let! test = generator cmd.generator ()

    return! Domain.nextTest cmd test |> Asyncresult.fromResult
}
let newTest (cmd:NewTestCommand): Asyncresult<DomainEvent list,Error> = asyncresult {
    let! artworks = MainIO.Storage.artworks ()
    let generator = MainIO.Randoms.testBuilder cmd.variants_count artworks >> IO.unwrapInsideAsync >> Asyncresult.ok
    let! test = generator ()

    return! Domain.newTest cmd (TestGenerator generator) test |> Asyncresult.fromResult
}
let guessResult (cmd:GuessResultCommand): Asyncresult<DomainEvent list,Error> = asyncresult {
    //let! artworks = MainIO.Storage.artworks ()
    //let generator = MainIO.Randoms.testBuilder cmd.variants_count artworks >> IO.unwrapInsideAsync >> Asyncresult.ok
    //let! test = generator ()

    //return! Domain.newTest cmd (TestGenerator generator) test |> Asyncresult.fromResult
    return! Domain.guessResult cmd |> Asyncresult.fromResult //cmd. (TestGenerator generator) test 
}
let matchCommand: CommandMatcher = 
    CommandMatcher (function
    |GuessResult cmd    -> SubscriptionId cmd.sub_id, fun () -> guessResult cmd
    |NewTest cmd        -> SubscriptionId cmd.sub_id, fun () -> newTest     cmd
    |NextTest cmd       -> SubscriptionId cmd.sub_id, fun () -> nextTest    cmd

    )