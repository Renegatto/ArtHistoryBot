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
type NotifyUserCommand = {
    notification : string
}

type Command = 
    |GuessResult of GuessResultCommand
    |NewTest of NewTestCommand
    |NextTest of NextTestCommand
    |NotifyUser of NotifyUserCommand

type Commands = Commands of Command  list

type CommandProcessor = unit -> AResult<DomainEvent list,Error>
type CommandMatcher = CommandMatcher of (Command -> CommandProcessor)
