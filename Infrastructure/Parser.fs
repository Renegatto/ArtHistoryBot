module Parser
open Domain
open FSharpPlus

type UnvalidatedArgset = UnvalidatedArgset of string list
type ValidArgset = ValidArgset of string list
type InvalidArgset = InvalidArgset of string list
type ValidatedArgset =
    |Valid of ValidArgset
    |Invalid of InvalidArgset

let linesToArgsets (args_separator:string): string list -> string list list =
        String.split (seq {yield args_separator}) 
        >> Seq.toList
        |> List.map
let validateArgset (UnvalidatedArgset argset): ValidatedArgset = 
    match List.length argset >= Constants.artworkFieldsCount with
    |true -> argset |> ValidArgset |> Valid
    |false -> argset |> InvalidArgset |> Invalid

let validArgsets (argsets:ValidatedArgset list): ValidArgset list =
    let filter: ValidatedArgset -> bool = function 
        |(Valid (ValidArgset _)) -> true 
        |(Invalid (InvalidArgset _)) -> false

    List.filter filter argsets
    |> List.map (function (Valid x) -> x)
let artwork (ValidArgset argset): Artwork =
    {
        Artwork.image   = List.item 0 argset
        author          = List.item 1 argset
        about           = List.item 2 argset
    }

