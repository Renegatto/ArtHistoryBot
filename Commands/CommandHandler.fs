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
type StoredEvent = (DomainTypes.SubscriptionId * Event)
type R'<'a> = Result<'a,Errors.Error>
type R<'a> = Asyncresult<'a,Errors.Error>
type CommandHandler() = //CommandHandler of (Command -> Result<DomainEvent,Error>)
    static member private handleCommand
        (CommandMatcher processor)
        (sid: SubscriptionId)
        (command:Command)
        :StoredEvent list -> (SubscriptionId * DomainEvent) list R = fun events -> 
        asyncresult {

            //let! stored_data = Subscriptions.readData sid
            let! events = processor command ()// stored_data

            let identified_events = List.map (fun event -> sid,event) events

            return identified_events
        }
    static member handle = CommandHandler.handleCommand
    //static member observe =  implement IObservable interface, 
    //that will respond to Subscriptions changing and notify subcriber
