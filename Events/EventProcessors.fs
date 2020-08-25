module EventProcessors
open Infrastructure
open Events
open Errors
open EventHandler
open Infrastructure.Operators



type R<'a> = AResult<'a,Errors.Error>
type R'<'a> = Result<'a,Errors.Error>
type StoredEvent = DomainTypes.SubscriptionId * Event
type EventListFold<'a> = StoredEvent list -> 'a
type Command = Commands.Command

let bot_speech s = ">>>bot>>>: " + s

let completeFromEventHub 
    (eventhub_query: DomainTypes.SubscriptionId -> 'data R' EventListFold) 
    (selector:       'data -> Command list R')
    (sid:            DomainTypes.SubscriptionId) 
    (events: StoredEvent list): Command list R' =

    selector =<< eventhub_query sid events

let nextQuizStarted (event:NewQuizStartedEvent  ) sid: Command list R' EventListFold =
    Ok [Commands.NotifyUser {notification = bot_speech "new quiz started"}] |> constant
let testFailed      (event:TestFailedEvent      ) sid: Command list R' EventListFold =
    Ok [Commands.NotifyUser {notification = bot_speech "test failed"}] |> constant
let testSolved      (event:TestSolvedEvent      ) sid: Command list R' EventListFold =
    Ok [Commands.NotifyUser {notification = bot_speech "test solved"}] |> constant
let testSended      (event:TestSendedEvent      ) sid: Command list R' EventListFold =
    Ok [
            Commands.NotifyUser {notification = bot_speech "test sended"};
            Commands.ShowTest {test = event.test} 
       ] |> constant
let testShowed      (event:TestShowedEvent      ) sid: Command list R' EventListFold =
    Ok [] |> constant

let userNotified    (event:UserNotifiedEvent    ) sid: Command list R' EventListFold = 
    Ok [] |> constant

let ( &&& ) f1 f2 x = (f1 x, f2 x)

let nextTest generator = [Commands.NextTest {generator=generator}]

let answerAttempt variant (test,hasUnfinished) : Command list R' =

    let error = Errors.NotUnpassedTestsFound |> Errors.Domain

    Result.result id error hasUnfinished
    *> Ok [ Commands.GuessResult {test=test; answer=variant}]

let userSentMessage 
    (event:UserSentMessageEvent)
    (sid:DomainTypes.SubscriptionId)
    (events:StoredEvent list): Command list R' =

    let matchInstruction:Parser.Userinput.Instruction ->
        DomainTypes.SubscriptionId ->
        StoredEvent list ->
        Command list R' = 

        function
        |Parser.Userinput.New (variants,tests) -> 
            fun _ _ -> 
                Ok [Commands.NewTest {variants_count=variants}]

        |Parser.Userinput.Next                 -> 
           completeFromEventHub EventHub.lastTestGeneratorOf (nextTest >> Ok)
 
        |Parser.Userinput.Answer (variant)     ->
            let eventHubQuery (sid:DomainTypes.SubscriptionId) = 
                EventHub.lastTestOf sid &&& (EventHub.hasUnfinishedTests sid >> Ok)
                >> Result.seqPair
            in completeFromEventHub eventHubQuery (answerAttempt variant)

    let processInstruction =
        matchInstruction >> (|>) sid >> (|>) events 
        >> List.seqResult

    Parser.Userinput.instructions_from_line event.message
    |> List.collect processInstruction
    |> Result.seqList
    |> Result.map //notifying about each parsed command
        (let notify msg = [Commands.NotifyUser {notification = msg}]
         let msg = bot_speech << sprintf "%A commands parsed" in
         fun xs -> 
             List.append (List.length xs |> msg |> notify) xs)

let userEditMessage sid events: Command list R' = Ok []

let matchDomainEvent = function
        |Events.NewQuizStarted event    -> nextQuizStarted event
        |Events.TestFailed event        -> testFailed event
        |Events.TestSolved event        -> testSolved event
        |Events.TestSended event        -> testSended event
        |Events.UserNotified event      -> userNotified event
        |Events.TestShowed event        -> testShowed event

let matchExternalEvent = function
        |Events.UserSentMessage event   -> userSentMessage event
        |Events.UserEditMessage         -> userEditMessage
let matchEvent: EventMatcher =
    EventMatcher (function
        |Events.Domain event    -> matchDomainEvent     event
        |Events.External event  -> matchExternalEvent   event)
