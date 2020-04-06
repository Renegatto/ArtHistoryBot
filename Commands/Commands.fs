module Commands
open CommandHandler
open Domain
open Errors
open Events
open Infrastructure
open MainIO

let nextTest (NextTest cmd): Asyncresult<DomainEvent,Error> = asyncresult {
    let generator (TestGenerator gen) = gen
    let! test = generator cmd.generator () |> Asyncresult.okAsync

    return Domain.nextTest test
}
let newTest (NewTest cmd): Asyncresult<DomainEvent,Error> = asyncresult {
    let! artworks = Storage.artworks ()

    let generator (TestGenerator gen) = gen
    let! test = generator cmd.generator () |> Asyncresult.okAsync

    return Domain.nextTest test
}

[<System.Diagnostics.DebuggerDisplay("Commands: command matching")>]
let matchCommand: CommandMatcher = 
    CommandMatcher (function
    //|GuessResult cmd -> cmd.sub_id,0
    //|NewTest cmd -> cmd.sub_id, newTest
    |NextTest cmd -> cmd.sub_id, nextTest
    )