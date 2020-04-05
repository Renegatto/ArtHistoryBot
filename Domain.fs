module Domain
open FSharpPlus

type Image = string
type Variant = int
type VariantsCount = int
type Filename = string
type Artwork =
    {
        image : Image
        author : string
        about : string
    }
type AnswerVariant =
    {
        artwork : Artwork
        variant : Variant
    }
    static member sort (variants: AnswerVariant list): AnswerVariant list =
        List.sortBy (fun x -> x.variant) variants
type TestResult =
    |Success 
    |Fail of AnswerVariant
type Test =
    {
        right_variant : AnswerVariant
        all_variants : AnswerVariant list
    }

open Infrastructure
let attemptToGuess (test:Test) (variant:Variant): TestResult = 
    List.filter (fun x -> x.variant = variant) test.all_variants
    |> List.head
    |> fun x -> x = test.right_variant 
    |> function
        |true -> Success
        |false -> Fail test.right_variant

//let testBuilder (storage_name:Filename) (count:int IO): unit IO -> Test IO =
    //let file: = 