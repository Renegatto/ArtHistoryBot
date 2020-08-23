module EventHub
open DomainTypes
open Infrastructure
open Errors.EventHubErrors

type Event = Events.Event
type Error = Errors.Error
type R<'a> = Asyncresult<'a,Errors.Error>
type R'<'a> = Result<'a,Error>
type StoredEvent = SubscriptionId * Event

module DataSelectors =
    let testFromEvent (event:Event): Test option =
        match event with
        |Events.Domain event ->
            match event with
            |Events.TestSolved event -> Some event.test
            |Events.TestFailed event -> Some event.test
            |Events.TestSended event -> Some event.test
            |_ -> None
        |_ -> None

    let generatorFromEvent (event:Event): TestGenerator option =
        match event with
        |Events.Domain event ->
            match event with
            |Events.NewQuizStarted event -> Some event.generator
            |_ -> None
        |_ -> None   

let sidInt (SubscriptionId x) = x

let eventHub: StoredEvent [] = Array.empty
let readEventHub () = Array.toList eventHub

let eventSid : StoredEvent -> SubscriptionId  = fst
let event    : StoredEvent -> Event           = snd
let events   : StoredEvent list -> Event list = List.map event

type EventListFold<'a> = StoredEvent list -> 'a
type EventListMapper<'a> = StoredEvent list -> 'a list
type EventListFolder<'a> = StoredEvent list -> 'a list
type EventListFilter = StoredEvent list -> StoredEvent list

let eventsInfoOf sid: StoredEvent -> bool = eventSid >> (=) sid

let filterInfo (filter: StoredEvent -> StoredEvent option): EventListFilter =
    List.choose filter  

let findInfo filter = filterInfo filter >> List.tryHead

let eventsInfosOf sid: StoredEvent EventListFolder =
    filterInfo (eventsInfoOf sid |> tryMatch)

let eventsOf (sid:SubscriptionId): Event EventListFolder =
    eventsInfosOf sid >> (List.unzip >> snd)

let extractFromEventsOf (selector: Event -> 'data option) (sid: SubscriptionId): 'data EventListFolder =
    eventsOf sid >> List.choose selector

let testsOf (sid:SubscriptionId): Test EventListFolder = 
    extractFromEventsOf DataSelectors.testFromEvent sid

let testGeneratorsOf (sid:SubscriptionId): TestGenerator EventListFolder =
    extractFromEventsOf DataSelectors.generatorFromEvent sid

let lastTestOf (sid:SubscriptionId): Test R' EventListFold =
    let error = 
        NotFoundError (sidInt sid, Some "lastTestOf")
        |> NotFound |> Errors.EventHub
    in testsOf sid >> List.tryHead >> Result.fromOption error

let lastTestGeneratorOf (sid:SubscriptionId):TestGenerator R' EventListFold = 
    let error = 
        NotFoundError (sidInt sid, Some "lastTestGeneratorOf")
        |> NotFound |> Errors.EventHub
    in testGeneratorsOf sid >> List.tryHead >> Result.fromOption error

type Observer<'a> = System.IObserver<'a>
type Observable<'a> = System.IObservable<'a>

type Unsubscribe(index:int, xs:_ option [], unsubscribes:int option [] ref) =

    interface System.IDisposable with
        member _.Dispose() =
            Array.set xs index None
            InterfaceTools.addInto unsubscribes index
            |> ignore
open FSharpPlus
type EventHub() =

    let eventHub: StoredEvent [] ref = ref Array.empty

    let observers: StoredEvent Observer option [] ref = ref [||]
    let unsubscibes: int option [] ref = ref [||]

    member _.read (): StoredEvent list R =
        Array.toList eventHub.contents
        |> Asyncresult.ok

    member o.filterData (filter: StoredEvent -> 'data option): 'data list R =
         o.read () |> Asyncresult.map (List.choose filter)  

    member o.processData (f:StoredEvent list -> 'data):'data R = // StoredEventFold a -> R a
        o.read () |> Asyncresult.map f

    member o.push (event:StoredEvent): unit R =
        printfn "EHub got event %A" event//"i were pushed with %A" observers.contents
        eventHub.contents <- Array.append [|event|] eventHub.contents
        o.notifyAll event
        |> Asyncresult.ok

    member o.publishDomain (events:(SubscriptionId * Events.DomainEvent) list): unit R =
        List.map (fun (id,event) -> id,Events.Domain event) events
        |> List.fold (o.push >> Asyncresult.next |> flip) (Asyncresult.ok ())
        |> ignore |> Asyncresult.ok 

    member o.notifyAll value =
        let fn (x:StoredEvent Observer) = x.OnNext value 
        Array.map (Option.map fn) observers.contents |> ignore

    interface System.IObservable<StoredEvent> with
        member _.Subscribe (observer:System.IObserver<StoredEvent>) =
            let id = InterfaceTools.addInto observers observer
            //printfn "they tried to subscribe: %A, and %A" observers.contents id
            new Unsubscribe(id,observers.contents,unsubscibes) :> System.IDisposable