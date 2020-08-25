module CommandHub
open Infrastructure
open DomainTypes
open Commands
open Errors.CommandHubErrors

type R<'a> = AResult<'a,Errors.Error>
type StoredCommand = SubscriptionId * Commands.Command 
type HubbedCommand =
    |Processed of StoredCommand
    |NotProcessed of StoredCommand
type Unsubscribe(index:int, xs:_ option [], unsubscribes:int option [] ref) =

    interface System.IDisposable with
        member _.Dispose() =
            Array.set xs index None
            InterfaceTools.addInto unsubscribes index
            |> ignore
open Infrastructure.Operators
type CommandHub() =
    
    let commandHub: StoredCommand [] ref = ref Array.empty

    let observers: (System.IObserver<StoredCommand> option) [] ref = ref [||]
    let unsubscibes: int option [] ref = ref [||]

    member _.read (): StoredCommand list R =
        Array.toList commandHub.contents
        |> AResult.ok     

    member o.push (command:StoredCommand): unit R =
        commandHub.contents <- Array.append [|command|] commandHub.contents
        //printfn "CHub got command %A" command
        o.notifyAll command
        |> AResult.ok

    member o.processData (f:StoredCommand list -> 'data):'data R = // StoredEventFold a -> R a
        f <!> o.read ()
    
    member o.publish (commands:StoredCommand list): unit R =
        //printfn "CH got some command to publish %A" commands
        List.map o.push commands
        |> AResult.sequential
        |> AResult.endpoint

    member _.notifyAll value =
        let fn (x: System.IObserver<StoredCommand>) = x.OnNext value 
        Array.map (Option.map fn) observers.contents
        |> ignore

    interface System.IObservable<StoredCommand> with
        member _.Subscribe observer =
            let id = InterfaceTools.addInto observers observer
            //printfn "observers of CH: %A ------------" observers
            new Unsubscribe(id,observers.contents,unsubscibes) :> System.IDisposable

