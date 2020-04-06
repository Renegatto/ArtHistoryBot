module MainIO
type TestGenerator = TestGenerator of (unit -> Domain.Test Async)

module Constructors =
    open Domain

    let variant index artwork = {AnswerVariant.artwork=artwork; variant=index}

    let test right others = {Test.right_variant=right; all_variants=others}

module Storage =
    open System
    open System.IO
    open Domain

    let content (): Async<string list> = async {
        let read = new Func<string []>( fun () -> File.ReadAllLines Constants.storageName )
        let! content = Async.FromBeginEnd(read.BeginInvoke,read.EndInvoke)
        return content |> Array.toList
    }

    let artworks (): Async<Artwork list> = async {
        let! lines = content ()
        return Parser.artworks_from_lines lines
    }

module Randoms =
    open System
    open Domain
    open Infrastucture
    open FSharpPlus

    let rng = Random(Constants.random_seed)

    let element (xs: 'a list) (): 'a IO =
        rng.Next (0, List.length xs)
        |> flip List.item xs
        |> IO

    let sample count (xs: 'a list) () : 'a list IO =
        IO.traverse (fun _ -> element xs ()) [1..count]

    let artwork :Artwork list -> unit -> Artwork IO = element

    let variant :AnswerVariant list -> unit -> AnswerVariant IO = element

    let variants count (artworks:Artwork list) (): AnswerVariant list IO = io {
        let! random_artworks = sample count artworks ()
        let enumerated = List.zip [0..List.length random_artworks] random_artworks
        return List.map (fun (i,artwork) -> Constructors.variant i artwork) enumerated
    }

    let testBuilder (variants_count: int) (artworks:Artwork List) (): Test IO = 
        io {
            let! all_variants = variants variants_count artworks ()
            let! right_variant = variant all_variants ()
            let test = Constructors.test right_variant all_variants
            return test
        }
module Subscriptions =
    open Infrastucture
    open FSharpPlus
    open Errors
    type SubscriptionId = SubscriptionId of int
    type SubscriptionError = Errors.SubscriptionError
    type StoredData =
        |TestData of TestGenerator
        |NoData
    type Subscription = {
        sid : SubscriptionId
        meta : string
        data : StoredData
    }
    let mutable subscriptions: Subscription [] = Array.empty
    let mutable releases : int[] = Array.empty

    let sid_2_int (SubscriptionId x) = x
    let tryFind (sid:SubscriptionId): Asyncresult<Subscription,Error> = asyncresult {
        match Array.tryFind (fun sub -> sub.sid = sid) subscriptions with
        |Some x -> 
            return x
        |None ->
            return! sid_2_int sid |> NoSubscriptionFound |> Subscription |> Asyncresult.error
    }
    let subscribe (): Asyncresult<Subscription,_> =
        match Array.tryHead releases with
        |Some release -> 
            releases <- Array.tail releases
            Array.set subscriptions release { Subscription.sid=SubscriptionId release; meta=""; data=NoData }
        |None ->
            let sid = Array.length subscriptions + 1
            Array.set subscriptions sid { Subscription.sid=SubscriptionId sid; meta=""; data=NoData }

        Array.last subscriptions |> Asyncresult.ok

    let unsubscribe (sid:SubscriptionId): Asyncresult<SubscriptionId,Error> = asyncresult {
        let! sub = tryFind sid
        releases <- Array.append [|sid_2_int sub.sid|] releases
        return sid
    }
    let storeData (sid:SubscriptionId) (data:StoredData): Asyncresult<Subscription,Error> = asyncresult {
        let! sub = tryFind sid
        Array.set subscriptions (sid_2_int sub.sid) {subscriptions.[sid_2_int sub.sid] with data=data}
        return sub
    }
    let readData (sid:SubscriptionId): Asyncresult<StoredData,Error> = asyncresult {
        let! sub = tryFind sid
        return sub.data
    }
//open System
open Domain
open Infrastucture

let testBuilder (variants_count:int) () : Test Async = async {
    let! artworks = Storage.artworks ()
    return IO.unwrapInsideAsync <|  Randoms.testBuilder variants_count artworks ()
}
