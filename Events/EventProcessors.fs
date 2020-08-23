module EventProcessors
open Infrastructure
open Events
open Errors
open EventHandler

type R<'a> = AResult<'a,Errors.Error>
type R'<'a> = Result<'a,Errors.Error>
type StoredEvent = DomainTypes.SubscriptionId * Event
type EventListFold<'a> = StoredEvent list -> 'a
type Command = Commands.Command

let bot_speech s = ">>>bot>>>: " + s

let nextQuizStarted (event:NewQuizStartedEvent  ) sid: Command list R' EventListFold =
    Ok [Commands.NotifyUser {notification = bot_speech "new quiz started"}] |> constant
let testFailed      (event:TestFailedEvent      ) sid: Command list R' EventListFold =
    Ok [Commands.NotifyUser {notification = bot_speech "test failed"}] |> constant
let testSolved      (event:TestSolvedEvent      ) sid: Command list R' EventListFold =
    Ok [Commands.NotifyUser {notification = bot_speech "test solved"}] |> constant
let testSended      (event:TestSendedEvent      ) sid: Command list R' EventListFold =
    Ok [Commands.NotifyUser {notification = bot_speech "test sended"}] |> constant
let userNotified    (event:UserNotifiedEvent    ) sid: Command list R' EventListFold = 
    Ok [] |> constant

let completeFromEventHub 
    (eventhub_query: DomainTypes.SubscriptionId -> 'data R' EventListFold) 
    (selector:       'data -> Command)
    (sid:            DomainTypes.SubscriptionId) 
    (events: StoredEvent list): Command R' =

    Result.map selector (eventhub_query sid events) 

let userSentMessage 
    (event:UserSentMessageEvent)
    (sid:DomainTypes.SubscriptionId)
    (events:StoredEvent list): Command list R' =

    let matchInstruction:Parser.Userinput.Instruction ->
        DomainTypes.SubscriptionId ->
        StoredEvent list ->
        Command R' = 

        function
        |Parser.Userinput.New (variants,tests) -> 
            Commands.NewTest {variants_count=variants} 
            |> Ok |> constant >> constant

        |Parser.Userinput.Next                 -> 
           fun generator -> Commands.NextTest {generator=generator}
           |> completeFromEventHub EventHub.lastTestGeneratorOf
 
        |Parser.Userinput.Answer (variant)     -> 
           fun test -> Commands.GuessResult {test=test; answer=variant}
           |> completeFromEventHub EventHub.lastTestOf

    Parser.Userinput.instructions_from_line event.message
    |> List.map (matchInstruction >> apply sid >> apply events) 
    |> Result.seqList
    |> Result.map 
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

let matchExternalEvent = function
        |Events.UserSentMessage event   -> userSentMessage event
        |Events.UserEditMessage         -> userEditMessage
let matchEvent: EventMatcher =
    EventMatcher (function
        |Events.Domain event    -> matchDomainEvent     event
        |Events.External event  -> matchExternalEvent   event)
