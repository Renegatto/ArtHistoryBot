module CommandProcessors
open CommandHandler
open Commands
open Domain
open DomainTypes
open Errors
open Events
open Infrastructure
open Infrastructure.Operators
type R<'a> = AResult<'a,Error>

let nextTest (cmd:NextTestCommand): DomainEvent list R =
    let generator (TestGenerator gen) = gen

    generator cmd.generator ()
    >>= (Domain.nextTest cmd >> AResult.fromResult)

let newTest (cmd:NewTestCommand): DomainEvent list R = 
    aresult {
        let! artworks = MainIO.Storage.artworks ()
        let generator () = 
                MainIO.Randoms.testBuilder cmd.variants_count artworks ()
                |> AResult.fromResult >>= AResult.okIO
        let! test = generator ()

        return! Domain.newTest cmd (TestGenerator generator) test |> AResult.fromResult
    }
let guessResult (cmd:GuessResultCommand): DomainEvent list R =
    Domain.guessResult cmd |> AResult.fromResult

let notifyUser (cmd:NotifyUserCommand): DomainEvent list R =
    printfn "%s" cmd.notification |> ignore
    AResult.ok [Events.UserNotified {notification = cmd.notification}]

let matchCommand: CommandMatcher = 
    CommandMatcher (constant << function
    |GuessResult cmd    -> guessResult cmd
    |NewTest cmd        -> newTest     cmd
    |NextTest cmd       -> nextTest    cmd
    |NotifyUser cmd     -> notifyUser  cmd
    )