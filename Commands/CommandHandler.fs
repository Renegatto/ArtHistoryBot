module CommandHandler
open Errors
open Domain
open DomainTypes
open Infrastructure
open MainIO
open Events
open Commands
//interface IRepository with
//    member 

type CommandHandler() = //CommandHandler of (Command -> Result<DomainEvent,Error>)
    static member private handleCommand
        (EventPublisher publish) 
        (CommandMatcher processor)
        (sid: SubscriptionId)
        (command:Command)
        : Asyncresult<DomainEvent list,Error> = asyncresult {

        let! stored_data = Subscriptions.readData sid
        let! events = processor command ()// stored_data

        List.map (fun event -> (sid,event)) events 
        |> publish |> ignore

        return events
    }
    static member handle = CommandHandler.handleCommand
    //static member observe =  implement IObservable interface, 
    //that will respond to Subscriptions changing and notify subcriber
  
let mutable commands = Commands Array.empty