module Parser
open Domain
open DomainTypes
open FSharpPlus
type UnvalidatedArgset  = UnvalidatedArgset of string list
type ValidArgset        = ValidArgset       of string list
type InvalidArgset      = InvalidArgset     of string list
type ValidatedArgset    =
    |Valid      of ValidArgset
    |Invalid    of InvalidArgset

let compressedSpaces (str:string) = 
    let not_double_space prev next = if prev = next && prev = " " then "" else next
    let folder (acc: {|prev:string; result:string|}) next = 
        let actual = not_double_space acc.prev next
        {| acc with prev = actual; result = sprintf "%s%s" acc.result actual|}

    String.split (seq{yield ""}) str
    |> Seq.toList
    |> List.fold folder {|prev = ""; result = "" |}
    |> fun x -> x.result

let linesToArgsets (args_separator:string): string list -> UnvalidatedArgset list =
    String.split (seq {yield args_separator}) 
    >> Seq.toList >> UnvalidatedArgset
    |> List.map

let validateArgset (predicate: string list -> bool) (UnvalidatedArgset argset): ValidatedArgset = 
    match predicate argset with
    |true -> argset |> ValidArgset |> Valid
    |false -> argset |> InvalidArgset |> Invalid

let validArgsets (argsets:ValidatedArgset list): ValidArgset list =
    let if_valid: ValidatedArgset -> ValidArgset option = function 
        |(Valid x) -> Some x
        |(Invalid _) -> None
    List.choose if_valid argsets

module Artworks =
    let artwork (ValidArgset argset): Artwork =
        {
            Artwork.image   = List.item 0 argset
            author          = List.item 1 argset
            about           = List.item 2 argset
        }
    let artworks_from_lines: string list -> Artwork list =
        let is_argsets_valid argset = List.length argset >= Constants.artworkFieldsCount

        linesToArgsets "," 
        >> List.map (validateArgset is_argsets_valid) 
        >> validArgsets 
        >> List.map artwork

module Userinput =
    type Instruction =
        |New    of variants:int * tests:int
        |Next
        |Answer of variant:int
    type InstructionParser =
        |New    of (string list -> Instruction)
        |Next   of (string list -> Instruction)
        |Answer of (string list -> Instruction)
    module Parsers =
        let new_quiz (args:string list) = Instruction.New (int args.[0], int args.[1])
        let next _ = Instruction.Next
        let answer: string list -> Instruction = List.item 0 >> int >> Instruction.Answer 
    type InstructionSignature = {
        name: string
        args_count : int
        parser : InstructionParser
    }
    let signatures = [
        { name = "new"    ; args_count = 2; parser = New     Parsers.new_quiz };
        { name = "next"   ; args_count = 0; parser = Next    Parsers.next     };
        { name = "answer" ; args_count = 2; parser = Answer  Parsers.answer   };
    ]
    open Infrastructure
    let (|ValidInstruction|_|) (word, potential_args): Option<Instruction * string list> =

        let predicate signature = 
            signature.name = word 
            && signature.args_count <= List.length potential_args

        option {
            let! matched_signature = List.tryFind predicate signatures
            let args_to_parse, remains = List.splitAt matched_signature.args_count potential_args
            let instruction =
                match matched_signature.parser with
                |New    parser -> parser
                |Next   parser -> parser
                |Answer parser -> parser
                <| args_to_parse
            return ValidInstruction (instruction, remains)
        }
    (*let my_super_list_fold (fn:'a list -> Option<'b * 'a list> ) (src: 'a list): 'b list =
        let rec do_stuff (acc:'b list) (remainder:'a list): 'b list * 'a list =
            match fn remainder with
            |Some (x,rem)   -> do_stuff (x::acc) rem
            |None           -> acc, remainder
        do_stuff List.empty src |> fst*)

    let instructions_from_line: string -> Instruction list =
        
        let rec collectInstructions (collected:Instruction list) (words: string list) =
            match words with
            |maybe_command :: other_words ->
                match maybe_command, other_words with
                |ValidInstruction (instruction, remainder) -> 
                    collectInstructions (instruction :: collected) remainder
                |_ -> collectInstructions collected other_words
            |[] -> List.rev collected

        String.trimWhiteSpaces 
        >> compressedSpaces 
        >> String.split (seq {yield " "})
        >> Seq.toList
       // >> my_super_list_fold lookup
        >> collectInstructions List.empty
            
