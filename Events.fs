module Events
open DomainTypes

type NewQuizStartedEvent =
    {
        sid : int
        generator : TestGenerator
        variants_count : int
    }
type TestSendedEvent =
    {
        sid : int
        test : Test
    }
type TestSolvedEvent =
    {
        sid : int
        test : Test
        answer : Variant
    }
type TestFailedEvent =
    {
        sid : int
        test : Test
        answer : Variant
    }

type DomainEvent =
    |TestSolved of TestSolvedEvent
    |TestFailed of TestFailedEvent
    |NewQuizStarted of NewQuizStartedEvent
    |TestSended of TestSendedEvent

type ExternalEvent =
    |UserSentMessage
    |UserEditMessage

