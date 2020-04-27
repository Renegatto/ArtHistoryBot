module Parser
open Domain
open DomainTypes
open FSharpPlus

type UnvalidatedArgset = UnvalidatedArgset of string list
type ValidArgset = ValidArgset of string list
type InvalidArgset = InvalidArgset of string list
type ValidatedArgset =
    |Valid of ValidArgset
    |Invalid of InvalidArgset

let linesToArgsets (args_separator:string): string list -> UnvalidatedArgset list =
        String.split (seq {yield args_separator}) 
        >> Seq.toList >> UnvalidatedArgset
        |> List.map
let validateArgset (UnvalidatedArgset argset): ValidatedArgset = 
    match List.length argset >= Constants.artworkFieldsCount with
    |true -> argset |> ValidArgset |> Valid
    |false -> argset |> InvalidArgset |> Invalid

let validArgsets (argsets:ValidatedArgset list): ValidArgset list =

    let if_valid: ValidatedArgset -> ValidArgset option = function 
        |(Valid x) -> Some x
        |(Invalid _) -> None

    List.choose if_valid argsets

let artwork (ValidArgset argset): Artwork =
    {
        Artwork.image   = List.item 0 argset
        author          = List.item 1 argset
        about           = List.item 2 argset
    }
let artworks_from_lines: string list -> Artwork list =
    linesToArgsets "," >> List.map (validateArgset) >> validArgsets >> List.map artwork
