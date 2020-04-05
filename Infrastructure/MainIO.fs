module MainIO
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
    let variants count (artworks:Artwork list) (): AnswerVariant list IO = io {
        let! random_artworks = sample count artworks ()
        let enumerated = List.zip [0..List.length random_artworks] random_artworks
        return List.map (fun (i,artwork) -> Constructors.variant i artwork) enumerated
    }
         
    let variant :AnswerVariant list -> unit -> AnswerVariant IO = element

    let testBuilder (variants_count: int) (artworks:Artwork List) (): Test IO = 
        io {
            let! all_variants = variants variants_count artworks ()
            let! right_variant = variant all_variants ()
            let test = Constructors.test right_variant all_variants
            return test
        }
    let testGenerator (variants_count:int) () : Test Async = async {
        let! artworks = Storage.artworks ()
        return IO.unwrapInsideAsync <| testBuilder variants_count artworks ()
    }