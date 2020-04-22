module MainIO
open Errors
open Infrastructure

module Constructors =
    open Domain
    open DomainTypes
    let variant index artwork = {AnswerVariant.artwork=artwork; variant=index}

    let test right others = {Test.right_variant=right; all_variants=others}

module Storage =
    open System
    open System.IO
    open Domain
    open DomainTypes
    open Errors
    open Infrastructure

    [<System.Diagnostics.DebuggerDisplay("Storage: content pulled")>]
    let content (): Async<string list> = async {
        let read = new Func<string []>( fun () -> File.ReadAllLines Constants.storageName )
        let! content = Async.FromBeginEnd(read.BeginInvoke,read.EndInvoke)
        return content |> Array.toList
    }
    [<System.Diagnostics.DebuggerDisplay("Storage: artworks pulled")>]
    let artworks (): Asyncresult<Artwork list,Error> = asyncresult {
        let! lines = content () |> Asyncresult.okAsync
        return Parser.artworks_from_lines lines
    }

module Randoms =
    open System
    open Domain
    open DomainTypes
    open Infrastructure
    open FSharpPlus

    let rng = Random(Constants.random_seed)
    let randomOrder _: int =
        match rng.Next(0,2) with
        |0 -> -1
        |1 ->  0
        |_ ->  1
    let shuffle<'a> :'a list -> IO<'a list> = 
        IO << List.sortBy randomOrder
    let element (xs: 'a list) (): 'a IO =
        rng.Next (0, List.length xs)
        |> flip List.item xs
        |> IO
    [<System.Diagnostics.DebuggerDisplay("Randoms: random sample touched")>]
    let sample count (xs: 'a list) () : 'a list IO = // позволяет выбирать несколько раз одно и то же
        shuffle xs |> IO.map (List.take count)
    [<System.Diagnostics.DebuggerDisplay("Randoms: random artwork touched")>]
    let artwork :Artwork list -> unit -> Artwork IO = element
    [<System.Diagnostics.DebuggerDisplay("Randoms: random variant touched")>]
    let variant :AnswerVariant list -> unit -> AnswerVariant IO = element
    [<System.Diagnostics.DebuggerDisplay("Randoms: variants touched")>]
    let variants count (artworks:Artwork list) (): AnswerVariant list IO = io {
        let! random_artworks = sample count artworks ()
        let result = List.indexed random_artworks
                     |> List.map (fun (i,artwork) -> Constructors.variant i artwork) 
        return result
    }
    [<System.Diagnostics.DebuggerDisplay("Randoms: test builder touched with count {variants_count}")>]
    let testBuilder (variants_count: int) (artworks:Artwork List) (): Test IO = 
        io {
            let! all_variants = variants variants_count artworks ()
            let! right_variant = variant all_variants ()
            let test = Constructors.test right_variant all_variants
            return test
        }
module Subscriptions =
    open Infrastructure
    open FSharpPlus
    open Commands
    type SubscriptionError = Errors.SubscriptionError
    type StoredData =
        |TestData of DomainTypes.TestGenerator
        |NoData
    type Subscription = {
        sid : SubscriptionId
        meta : string
        data : StoredData
    }
    let mutable subscriptions: Subscription [] = Array.empty
    let mutable releases : int[] = Array.empty

    let sid_2_int (SubscriptionId x) = x

    [<System.Diagnostics.DebuggerDisplay(": lookup for {sid}")>]
    let tryFind (sid:SubscriptionId): Asyncresult<Subscription,Error> = asyncresult {
        match Array.tryFind (fun sub -> sub.sid = sid) subscriptions with
        |Some x -> 
            return x
        |None ->
            return! sid_2_int sid |> NoSubscriptionFound |> Subscription |> Asyncresult.error
    }
    [<System.Diagnostics.DebuggerDisplay("Subscriptions: Subcription")>]
    let subscribe (): Asyncresult<Subscription,_> =
        match Array.tryHead releases with
        |Some release -> 
            releases <- Array.tail releases
            Array.set subscriptions release { Subscription.sid=SubscriptionId release; meta=""; data=NoData }
        |None ->
            let sid = Array.length subscriptions + 1
            Array.set subscriptions sid { Subscription.sid=SubscriptionId sid; meta=""; data=NoData }

        Array.last subscriptions |> Asyncresult.ok
    [<System.Diagnostics.DebuggerDisplay("Subscriptions: unsubscribing {sid}")>]
    let unsubscribe (sid:SubscriptionId): Asyncresult<SubscriptionId,Error> = asyncresult {
        let! sub = tryFind sid
        releases <- Array.append [|sid_2_int sub.sid|] releases
        return sid
    }
    [<System.Diagnostics.DebuggerDisplay("Subscriptions: store data for {sid}")>]
    let storeData (sid:SubscriptionId) (data:StoredData): Asyncresult<Subscription,Error> = asyncresult {
        let! sub = tryFind sid
        Array.set subscriptions (sid_2_int sub.sid) {subscriptions.[sid_2_int sub.sid] with data=data}
        return sub
    }
    [<System.Diagnostics.DebuggerDisplay("Subscriptions: read data for {sid}")>]
    let readData (sid:SubscriptionId): Asyncresult<StoredData,Error> = asyncresult {
        let! sub = tryFind sid
        return sub.data
    }
//open System
open Domain
open Infrastructure

//[<System.Diagnostics.DebuggerDisplay("MainIO: test builder touched {sid}")>]
//let testBuilder (variants_count:int) () : Asyncresult<Test,Error>  = asyncresult {
//    let! artworks = Storage.artworks ()
//    return IO.unwrapInsideAsync <|  Randoms.testBuilder variants_count artworks ()
//}
