module CommandHandler
open Errors
open Domain
open DomainTypes
open Infrastructure
open MainIO
open Events
open Commands


type CommandHandler = CommandHandler with //CommandHandler of (Command -> Result<DomainEvent,Error>)
    static member private handleCommand (command:Command) 
        (EventPublisher publish) 
        (CommandMatcher matcher)
        : Asyncresult<DomainEvent,Error> = asyncresult {

        let (sid,processor) = matcher command
        let! stored_data = Subscriptions.readData sid
        let! event = processor command // stored_data

        publish event |> ignore

        return event
    }
    static member handle = CommandHandler.handleCommand
    //static member observe =  implement IObservable interface, 
    //that will respond to Subscriptions changing and notify subcriber
  
let mutable commands = Commands Array.empty