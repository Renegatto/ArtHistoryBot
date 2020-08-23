module MainIO
open Errors
open Infrastructure
type R<'a> = AResult<'a,Error>
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
    open Infrastructure.Operators

    let content (): Async<string list> = async { //doesnt work on chief's computer ':D
        let read = new Func<string []> ( fun () -> File.ReadAllLines Constants.storageName )
        let! content = Async.FromBeginEnd(read.BeginInvoke,read.EndInvoke)
        return content |> Array.toList
    }
    let content_alt (): Async<string list> = async {
        return File.ReadAllLines Constants.storageName |> Array.toList
    }
    let artworks (): Artwork list R = aresult {
        let! lines = (*content*) content_alt ()|> AResult.okAsync
        return Parser.Artworks.artworks_from_lines lines
    }

module Randoms =
    open System
    open Domain
    open DomainTypes
    open Infrastructure.Operators

    let rng = Random(Constants.random_seed)
    let randomOrder _: int = rng.Next ()
    let shuffle<'a> :'a list -> IO<'a list> = 
        IO << List.sortBy randomOrder
    let element (xs: 'a list) (): 'a IO =
        rng.Next (0, List.length xs)
        |> flip List.item xs
        |> IO
    let sample count (xs: 'a list) () : 'a list IO = //fixed? позволяет выбирать несколько раз одно и то же
        List.take count <!> shuffle xs //|> IO.map ()
    let artwork :Artwork list -> unit -> Artwork IO = element
    let variant :AnswerVariant list -> unit -> AnswerVariant IO = element
    let variants count (artworks:Artwork list) (): AnswerVariant list IO = io {
        let! random_artworks = sample count artworks ()
        let result = List.indexed random_artworks
                     |> List.map (fun (i,artwork) -> Constructors.variant i artwork) 
        return result
    }
    let testBuilder (variants_count: int) (artworks:Artwork List) (): Result<Test IO,Error> = 
        let enoughArtworks = 
            match List.length artworks >= variants_count with
            |true -> Ok artworks
            |false -> 
                NotEnoughArtworksForTest {expected = variants_count; got = List.length artworks} 
                |> Domain |> Error

        let new_test artworks = io {
                let! all_variants = variants variants_count artworks ()
                let! right_variant = variant all_variants ()
                let test = Constructors.test right_variant all_variants
                return test
            }

        enoughArtworks
        |> Result.map new_test
(*
module Subscriptions =
    open Infrastructure
    open FSharpPlus
    open DomainTypes
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
            subscriptions <- Array.append subscriptions [|{ Subscription.sid=SubscriptionId sid; meta=""; data=NoData }|]
            ()

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
open Infrastructure
*)
//[<System.Diagnostics.DebuggerDisplay("MainIO: test builder touched {sid}")>]
//let testBuilder (variants_count:int) () : Asyncresult<Test,Error>  = asyncresult {
//    let! artworks = Storage.artworks ()
//    return IO.unwrapInsideAsync <|  Randoms.testBuilder variants_count artworks ()
//}
