module MainIO

module Strorage =
    open System
    open System.IO
    open Domain

    let artworks_from_lines lines : Artwork list =
        let args: string list list = Parser.linesToArgsets "," lines
        let validated: Parser.ValidatedArgset list = 
            List.map (Parser.UnvalidatedArgset>>Parser.validateArgset) args
        let valid: Parser.ValidArgset list = Parser.validArgsets validated

        List.map Parser.artwork valid

    let content (): Async<string list> = async {
        let read = new Func<string []>( fun () -> File.ReadAllLines Constants.storageName )
        let! content = Async.FromBeginEnd(read.BeginInvoke,read.EndInvoke)
        return content |> Array.toList
    }
    let artworks (): Async<Artwork list> = async {
        let! lines = content ()
        return artworks_from_lines lines
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