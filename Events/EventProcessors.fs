module EventProcessors
open Infrastructure
open Events
open Errors
(*type DomainEvent =
    |TestSolved of TestSolvedEvent
    |TestFailed of TestFailedEvent
    |NewQuizStarted of NewQuizStartedEvent
    |TestSended of TestSendedEvent*)

//let testSolved (event: TestSolvedEvent): Asyncresult<Commands.Command list,Error> = 0
//let testSolved (NewQuizStarted event) = 0