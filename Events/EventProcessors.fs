module EventProcessors
open Infrastructure
open Events
open Errors
open EventHandler

type R<'a> = Asyncresult<'a,Errors.Error>
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










(*type DomainEvent =
    |TestSolved of TestSolvedEvent
    |TestFailed of TestFailedEvent
    |NewQuizStarted of NewQuizStartedEvent
    |TestSended of TestSendedEvent*)
(*module External =
    let userSentMessage (event: UserSentMessageEvent):  Asyncresult<Commands.Command list,Error> =
        let (|Prefix|_|) (pre:string) (whole:string) =
            if whole.StartsWith(pre) then (Some<<whole.Substring) pre.Length else None
        let commands =
            match event.message with
            |Prefix "new quiz" args -> //need to move in Parser.UserInput.toCommand
                [Commands.NewTest {
                    Commands.NewTestCommand.sub_id = Storage.sidByUserID event.user_id
                    Commands.NewTestCommand.variants_count = read args
                }]
            |Prefix "answer is" args ->
                //let! test = last_test <| Storage.sidByUserID event.user_id

                [Commands.GuessResult {
                    Commands.GuessResultCommand.sub_id = Storage.sidByUserID event.user_id
                    Commands.GuessResultCommand.variants_count = read args
                }]
            |Prefix "done" args ->
            |_ ->


        asyncresult {
            return List.empty
        }
module Domain =
    let testSolved (event: TestSolvedEvent): Asyncresult<Commands.Command list,Error> = 
        asyncresult {
    
        //send discord message about solved test
        return List.empty }
    //let newQuizStarted (event: NewQuizStartedEvent): Asyncresult<Commands.Command list,Error> = 0
    //let testFailed (event: TestFailedEvent): Asyncresult<Commands.Command list,Error> = 0
    //let testSended (event: TestSendedEvent): Asyncresult<Commands.Command list,Error> = 0*)