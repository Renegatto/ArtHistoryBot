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

let showError (error:DomainError): DomainEvent list R =
    printfn ">>> an error occured: %A" error |> ignore
    AResult.ok []    
let showTest (cmd:ShowTestCommand): DomainEvent list R =
    let showed_test = Domain.showTest cmd

    Result.map (fun (x:Events.TestShowedEvent) -> printfn "%s" x.message) showed_test
    |> ignore

    AResult.fromResult showed_test
    |>> fun event -> [Events.TestShowed event]

let matchCommand: CommandMatcher = 
    CommandMatcher (constant << function
    |GuessResult cmd    -> guessResult cmd
    |NewTest cmd        -> newTest     cmd
    |NextTest cmd       -> nextTest    cmd
    |NotifyUser cmd     -> notifyUser  cmd
    |ShowError cmd      -> showError   cmd
    |ShowTest cmd       -> showTest    cmd
    )