module Commands
open Infrastructure
open Events
open Errors
open DomainTypes
type SubscriptionId = SubscriptionId of int

type GuessResultCommand = {
    sub_id : int
    test : Test
    answer : Variant
}
type NewTestCommand = {
    sub_id : int
    variants_count : int
}
type NextTestCommand = {
    sub_id : int
    generator : TestGenerator
}

type Command = 
    |GuessResult of GuessResultCommand
    |NewTest of NewTestCommand
    |NextTest of NextTestCommand

type EventPublisher = EventPublisher of (DomainEvent list -> unit Async)
type Commands = Commands of Command []
type CommandProcessor = (Command -> Asyncresult<DomainEvent list,Error>)
type CommandMatcher = CommandMatcher of (Command -> SubscriptionId*CommandProcessor)