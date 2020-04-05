module MainIO

module Storage =
    open System
    open System.IO
    open Domain
    open Infrastucture

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

    let element (xs: 'a list) () : 'a IO =
        Random(25097).Next (0, List.length xs)
        |> flip List.item xs
        |> IO

    let artwork :Artwork list -> unit -> Artwork IO = element
    let variant :AnswerVariant list -> unit -> AnswerVariant IO = element

    //let test (variants_count: int) (artworks:Artwork List) (_:unit IO) : Test IO =