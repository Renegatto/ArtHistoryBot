module EventProcessors
open Infrastructure
open Events
open Errors

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