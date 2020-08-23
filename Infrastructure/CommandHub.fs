module CommandHub
open Infrastructure
open DomainTypes
open Commands
open Errors.CommandHubErrors

type R<'a> = Asyncresult<'a,Errors.Error>
type StoredCommand = SubscriptionId * Commands.Command 
type HubbedCommand =
    |Processed of StoredCommand
    |NotProcessed of StoredCommand

//let mutable commandHub : HubbedCommand [] = Array.empty
//let mutable commandHub': StoredCommand list = []

type Unsubscribe(index:int, xs:_ option [], unsubscribes:int option [] ref) =

    interface System.IDisposable with
        member _.Dispose() =
            Array.set xs index None
            InterfaceTools.addInto unsubscribes index
            |> ignore

type CommandHub() =

    let commandHub: StoredCommand [] ref = ref Array.empty

    let observers: (System.IObserver<StoredCommand> option) [] ref = ref [||]
    let unsubscibes: int option [] ref = ref [||]

    member _.read (): StoredCommand list R =
        Array.toList commandHub.contents
        |> Asyncresult.ok     

    member o.push (command:StoredCommand): unit R =
        commandHub.contents <- Array.append [|command|] commandHub.contents
        printfn "CH got some command %A" command
        o.notifyAll command
        |> Asyncresult.ok

    member o.processData (f:StoredCommand list -> 'data):'data R = // StoredEventFold a -> R a
        o.read () |> Asyncresult.map f

    member o.publish (commands:StoredCommand list): unit R =
        printfn "CH got some command to publish %A" commands
        List.map o.push commands 
        |> ignore |> Asyncresult.ok 

    member _.notifyAll value =
        let fn (x: System.IObserver<StoredCommand>) = x.OnNext value 
        Array.map (Option.map fn) observers.contents
        |> ignore

    interface System.IObservable<StoredCommand> with
        member _.Subscribe observer =
            let id = InterfaceTools.addInto observers observer
            new Unsubscribe(id,observers.contents,unsubscibes) :> System.IDisposable


(*
let storedCommand = function Processed cmd -> cmd |NotProcessed cmd -> cmd 

let push (value:StoredCommand): unit R =
    do commandHub <- Array.append [|NotProcessed value|] commandHub

    Asyncresult.ok ()

let modify (id:int): HubbedCommand -> unit R =
    match Array.tryItem id commandHub with
    |Some _ ->
        Array.set commandHub id >> Asyncresult.ok
    |None -> 
        IndexNotFound (id, Some "cannot modify: no item exists")
        |> Errors.CommandHub |> Asyncresult.error |> constant

let lastNonProcessed (): (int * StoredCommand) R =
    Array.tryFindIndex (function NotProcessed _ -> true |_ -> false) commandHub
    |> function
        |Some index ->
            let elem = Array.get commandHub >> storedCommand in
            Asyncresult.ok (index,elem index)
        |None -> 
            NoNotProcessedFound |> Errors.CommandHub |> Asyncresult.error
    
let processEvent (): StoredCommand R =
    lastNonProcessed ()
    |> let change_and_mark (id,event) = 
            do modify id (Processed event) |> ignore
            event 
       in Asyncresult.map change_and_mark

let publish (events_info : StoredCommand list): unit R =
    List.map push events_info |> ignore
    Asyncresult.ok () *)



