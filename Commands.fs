module Commands
open Infrastructure
open Events
open Errors
open DomainTypes


type GuessResultCommand = {
    test : Test
    answer : Variant
}
type NewTestCommand = {
    variants_count : int
}
type NextTestCommand = {
    generator : TestGenerator
}

type Command = 
    |GuessResult of GuessResultCommand
    |NewTest of NewTestCommand
    |NextTest of NextTestCommand

type EventPublisher = EventPublisher of ((SubscriptionId * DomainEvent) list -> Asyncresult<unit,Error>)
type Commands = Commands of Command []

type CommandProcessor = unit -> Asyncresult<DomainEvent list,Error>
type CommandMatcher = CommandMatcher of (Command -> CommandProcessor)
