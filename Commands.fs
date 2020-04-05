module Commands
type DomainEvent = DomainEvent
type Error = Error of string

type Command =
    |GuessResult
    |NewTest
type CommandHandler = CommandHandler of (Command -> Result<DomainEvent,Error>)

// CH := cmd -> dom_event | error