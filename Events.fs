module Events
open DomainTypes

type NewQuizStartedEvent =
    {
        generator : TestGenerator
        variants_count : int
    }
type TestSendedEvent =
    {
        test : Test
    }
type TestSolvedEvent =
    {
        test : Test
        answer : Variant
    }
type TestFailedEvent =
    {
        test : Test
        answer : Variant
    }
type UserNotifiedEvent =
    {
        notification : string
    }
type TestShowedEvent =
    {
        test: Test
        message: string
    }
type DomainEvent =
    |TestSolved of TestSolvedEvent
    |TestFailed of TestFailedEvent
    |NewQuizStarted of NewQuizStartedEvent
    |TestSended of TestSendedEvent
    |UserNotified of UserNotifiedEvent
    |TestShowed of TestShowedEvent

type UserSentMessageEvent =
    {
        user_id : int
        message : string
    }
type ExternalEvent =
    |UserSentMessage of UserSentMessageEvent
    |UserEditMessage

type Event =
    |Domain of DomainEvent
    |External of ExternalEvent