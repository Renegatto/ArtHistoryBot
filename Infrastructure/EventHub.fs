module EventHub
open DomainTypes
open Infrastructure
open Errors.EventHubErrors

type Event = Events.Event
type Error = Errors.Error
type R<'a,'b> = Asyncresult<'a,'b>
type StoredEvent = SubscriptionId * Event

let sidInt (SubscriptionId x) = x

module Internals =
    let mutable eventHub: StoredEvent list = []

    let eventSid : StoredEvent -> SubscriptionId  = fst
    let event    : StoredEvent -> Event           = snd
    let events   : StoredEvent list -> Event list = List.map event

    let eventsInfoOf sid : R<StoredEvent list, Error> =
        List.filter (eventSid >> (=) sid) eventHub
        |> Asyncresult.ok

    let filterInfo (filter: StoredEvent -> StoredEvent option): StoredEvent list =
        List.choose filter eventHub
    
    let findInfo = filterInfo >> List.tryHead

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

let publish (events_info : StoredEvent list): R<unit,Error> =
    Internals.eventHub <- List.append Internals.eventHub events_info
    Asyncresult.ok ()

let eventsOf : SubscriptionId -> R<Event list,Error> =
    Internals.eventsInfoOf >> Asyncresult.map (List.unzip >> snd)

let extractFromEventsOf (selector: Event -> 'data option): SubscriptionId -> R<'data list,Error> =
    eventsOf
    >> Asyncresult.map (List.choose selector) 

let testsOf : SubscriptionId -> R<Test list,Error> = 
    extractFromEventsOf DataSelectors.testFromEvent 

let testGeneratorsOf : SubscriptionId -> R<TestGenerator list,Error> =
    extractFromEventsOf DataSelectors.generatorFromEvent 

let lastFromExtractedOf (selector: SubscriptionId -> R<'data list,Error>) 
    (error_details: string option)
    (sid: SubscriptionId): R<'data,Error> = 

    asyncresult {
        let! generators = selector sid
        let! result = 
            match List.rev generators |> List.tryHead with
            |Some test -> Asyncresult.ok test
            |None -> 
                NotFoundError (sidInt sid, error_details)
                |> NotFound |> Errors.EventHub
                |> Asyncresult.error
        return result
    }

let lastTestOf : SubscriptionId -> R<Test,Error> = 
    lastFromExtractedOf testsOf (Some "lastTestOf")

let lastTestGeneratorOf : SubscriptionId -> R<TestGenerator,Error> = 
    lastFromExtractedOf testGeneratorsOf (Some "lastTestGeneratorOf")


//let testGeneratorsOf

// a x* b x * c x * d x