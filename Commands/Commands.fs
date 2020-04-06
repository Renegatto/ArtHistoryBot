module Commands
open CommandHandler
open Domain
open Errors
open Events
open Infrastructure

let nextTest (NextTest cmd): Asyncresult<DomainEvent,Error> = asyncresult {
    let generator (MainIO.TestGenerator gen) = gen
    let! test = generator cmd.generator ()

    return Domain.nextTest cmd test
}
let newTest (NewTest cmd): Asyncresult<DomainEvent,Error> = asyncresult {
    let! artworks = MainIO.Storage.artworks ()
    let generator = MainIO.Randoms.testBuilder cmd.variants_count artworks >> IO.unwrapInsideAsync
    let test = generator ()

    return Domain.newTest cmd generator test
}

[<System.Diagnostics.DebuggerDisplay("Commands: command matching")>]
let matchCommand: CommandMatcher = 
    CommandMatcher (function
    //|GuessResult cmd -> cmd.sub_id,0
    //|NewTest cmd -> cmd.sub_id, newTest
    |NextTest cmd -> cmd.sub_id, nextTest
    )